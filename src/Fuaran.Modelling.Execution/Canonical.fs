// SPDX-License-Identifier: Apache-2.0
// Copyright 2026 Diametrical Ltd

namespace Fuaran.Modelling.Execution

// ============================================================================
//  The canonical encoding (§3), and the minting canonicalisation (§4.5).
//
//  Canonical means: given the same values, every conformant emitter produces the
//  same BYTES, so a hash over those bytes is a stable identity. Identity is the
//  whole point of the interface, so the rules are implemented exhaustively here
//  rather than delegated to a library — and deliberately NOT to the substrate's own
//  canonical wire discipline, which differs from this one in four ways that
//  would each have been a silent, hash-changing divergence:
//
//    1. this encoding orders a RECORD's members by declaration, not
//       lexicographically (§3.2 divergence 1 — "the divergence most likely to
//       bite, so check it first");
//    2. it uses JSON's short escapes (`\t`, `\n`, …) where they exist, rather
//       than `\u00xx` for every control character (§3.1 rule 6);
//    3. its real numbers follow the ECMAScript `Number::toString` algorithm,
//       which is not the .NET round-trip layout — `1e-7` against `1E-07`
//       (§3.1 rule 9);
//    4. it REQUIRES `null` for an absent optional (§3.1 rule 5), where the
//       substrate's model has no null at all.
//
//  Each of those is invisible until a digest disagrees across two parties, at
//  which point nothing in either record says why. That is why this file exists.
// ============================================================================

/// A JSON value as READ. Distinct from the emitter's model below because reading and
/// writing are asymmetric here: a reader must hold `null` and must preserve duplicate
/// members (so the minting rule can refuse them), where an emitter produces neither.
type JsonValue =
    | JsonNull
    | JsonBool of bool
    | JsonString of string
    /// The parsed double AND the token it was written as. The token is kept because
    /// the minting rule's domain is stated in terms of what the RENDERING contains —
    /// an integer literal beyond exact double range is refused, and that is a fact
    /// about the token, not about the double it rounded to.
    | JsonNumber of value: float * token: string
    | JsonArray of JsonValue list
    | JsonObject of (string * JsonValue) list

/// The emitter's value model. The three composite shapes of §3.3 are three cases,
/// because they encode differently and nothing else about them differs.
type Doc =
    /// An absent optional. Always present as `null`, never omitted (§3.1 rule 5).
    | DNull
    | DBool of bool
    | DStr of string
    /// A 32-bit integer — a JSON number (§3.1 rule 7).
    | DInt of int
    /// A 64-bit integer — a sign-prefixed decimal STRING (§3.1 rule 8). A seed is not
    /// exactly representable as a double, and silently rounding the seed that
    /// reproduces a fit is worse than encoding it as text.
    | DI64 of int64
    /// A real number (§3.1 rule 9). Non-finite values MUST NOT appear on the wire.
    | DReal of float
    /// A record: members in DECLARATION order (§3.1 rule 3).
    | DRec of (string * Doc) list
    /// A map: members in ordinal-ascending key order (§3.1 rule 4).
    | DMap of (string * Doc) list
    /// A list: the order the emitter produced (§3.1 rule 12).
    | DList of Doc list

/// The §3 canonical writer and reader.
[<RequireQualifiedAccess>]
module Canonical =

    // ---- strings (§3.1 rule 6) ----------------------------------------------

    /// The characters the writer escapes: `"`, `\`, and everything below U+0020.
    let private escapeSet =
        System.Buffers.SearchValues.Create(
            System.ReadOnlySpan<char>([| yield '"'; yield '\\'; for c in 0..0x1f -> char c |])
        )

    /// The characters that end a clean run in the reader: `"` and `\`.
    let private stringStopSet =
        System.Buffers.SearchValues.Create(System.ReadOnlySpan<char>([| '"'; '\\' |]))

    /// The offset of the first character of `set` at or after `start` in `s`, or -1.
    let private indexOfAny (set: System.Buffers.SearchValues<char>) (s: string) (start: int) : int =
        let k =
            System.MemoryExtensions.IndexOfAny(System.MemoryExtensions.AsSpan(s, start), set)

        if k < 0 then -1 else start + k

    /// Escape `"`, `\`, and characters below U+0020 — using JSON's short escape where
    /// one exists and `\u00xx` where none does. Characters at or above U+0020,
    /// including all non-ASCII, are emitted literally.
    ///
    /// Almost every string holds nothing to escape, so the clean case is one scan and
    /// the input returned as it is; otherwise each clean run is copied whole between
    /// the characters that need an escape.
    let escape (s: string) : string =
        let first = indexOfAny escapeSet s 0

        if first < 0 then
            s
        else
            let sb = System.Text.StringBuilder(s.Length + 8)
            let mutable runStart = 0
            let mutable i = first

            while i >= 0 do
                sb.Append(s, runStart, i - runStart) |> ignore

                match s[i] with
                | '"' -> sb.Append "\\\"" |> ignore
                | '\\' -> sb.Append "\\\\" |> ignore
                | '\b' -> sb.Append "\\b" |> ignore
                | '\f' -> sb.Append "\\f" |> ignore
                | '\n' -> sb.Append "\\n" |> ignore
                | '\r' -> sb.Append "\\r" |> ignore
                | '\t' -> sb.Append "\\t" |> ignore
                | c -> sb.Append("\\u").Append((int c).ToString "x4") |> ignore

                runStart <- i + 1
                i <- indexOfAny escapeSet s runStart

            sb.Append(s, runStart, s.Length - runStart).ToString()

    /// Is every surrogate in this string paired? An unpaired surrogate has no UTF-8
    /// encoding, so it has no bytes to hash — which is why §4.5 puts it outside the
    /// minting rule's domain rather than encoding it as a replacement character.
    let isWellFormedUnicode (s: string) : bool =
        let rec go i =
            if i >= s.Length then
                true
            elif System.Char.IsHighSurrogate s[i] then
                i + 1 < s.Length && System.Char.IsLowSurrogate s[i + 1] && go (i + 2)
            elif System.Char.IsLowSurrogate s[i] then
                false
            else
                go (i + 1)

        go 0

    // ---- 64-bit integers (§3.1 rule 8) --------------------------------------

    /// A 64-bit integer as a decimal string with an EXPLICIT sign — including for
    /// zero, which is `"+0"`.
    let int64Text (v: int64) : string =
        // `abs Int64.MinValue` overflows, so take the sign off the rendering rather
        // than off the value.
        if v < 0L then string v else "+" + string v

    /// Read a sign-prefixed decimal string back. The sign is REQUIRED: a bare decimal
    /// is a different encoding, and accepting it here would let a non-conformant
    /// emitter through the one place that could have caught it.
    let tryParseInt64Text (s: string) : Result<int64, string> =
        if s.Length < 2 || (s[0] <> '+' && s[0] <> '-') then
            Error("expected a sign-prefixed 64-bit integer (§3.1 rule 8), got '" + s + "'")
        elif s |> Seq.skip 1 |> Seq.exists (fun c -> c < '0' || c > '9') then
            Error("expected a sign-prefixed 64-bit integer (§3.1 rule 8), got '" + s + "'")
        else
            let digits = s.Substring 1

            match System.Int64.TryParse((if s[0] = '-' then "-" + digits else digits)) with
            | true, v -> Ok v
            | _ -> Error("64-bit integer out of range: '" + s + "'")

    // ---- real numbers (§3.1 rule 9) -----------------------------------------

    /// The ECMAScript `Number::toString` algorithm — the shortest decimal that
    /// round-trips to the same IEEE-754 double, laid out ECMAScript's way.
    ///
    /// .NET's `"R"` format already gives the shortest round-tripping DIGITS; what it
    /// does not give is ECMAScript's LAYOUT of them, and the two disagree on exactly
    /// the values a specification corpus is written to catch: `1e-7` here against
    /// `1E-07` there, `1e+21` against `1E+21`. So the digits are taken from .NET and
    /// re-laid by the rule, rather than the rendering being taken wholesale.
    let real (v: float) : string =
        if System.Double.IsNaN v || System.Double.IsInfinity v then
            // §3.1 rule 9 — non-finite values MUST NOT appear on the wire. Reaching
            // here means an emitter tried; refusing loudly is the only honest answer,
            // because every alternative writes a document whose meaning is invented.
            invalidArg "v" "a non-finite real cannot be encoded (§3.1 rule 9)"
        elif v = 0.0 then
            // Collapses -0.0 to 0, as the rule requires.
            "0"
        else
            let negative = v < 0.0
            let magnitude = abs v

            let rendered =
                magnitude.ToString("R", System.Globalization.CultureInfo.InvariantCulture)

            // Split the .NET rendering into its digits and a base-10 exponent, in the
            // form `value = 0.<digits> * 10^point`.
            let mantissa, exponent =
                match rendered.IndexOf 'E' with
                | -1 -> rendered, 0
                | i -> rendered.Substring(0, i), int (rendered.Substring(i + 1))

            let integerPart, fractionPart =
                match mantissa.IndexOf '.' with
                | -1 -> mantissa, ""
                | i -> mantissa.Substring(0, i), mantissa.Substring(i + 1)

            let rawDigits = integerPart + fractionPart
            let leading = rawDigits.Length - rawDigits.TrimStart('0').Length
            let digits = (rawDigits.TrimStart '0').TrimEnd '0'
            let digits = if digits = "" then "0" else digits
            let point = integerPart.Length + exponent - leading
            let k = digits.Length
            let sign = if negative then "-" else ""

            let body =
                if k <= point && point <= 21 then
                    digits + System.String('0', point - k)
                elif 0 < point && point <= 21 then
                    digits.Substring(0, point) + "." + digits.Substring point
                elif -6 < point && point <= 0 then
                    "0." + System.String('0', -point) + digits
                else
                    let e = point - 1

                    let head =
                        if k = 1 then
                            digits
                        else
                            digits.Substring(0, 1) + "." + digits.Substring 1

                    head + "e" + (if e >= 0 then "+" else "-") + string (abs e)

            sign + body

    // ---- the writer ----------------------------------------------------------

    let private ordinal (a: string) (b: string) = System.String.CompareOrdinal(a, b)

    /// Render a document. No insignificant whitespace, anywhere (§3.1 rule 1).
    let render (d: Doc) : string =
        let sb = System.Text.StringBuilder()

        let rec go (d: Doc) =
            match d with
            | DNull -> sb.Append "null" |> ignore
            | DBool b -> sb.Append(if b then "true" else "false") |> ignore
            | DStr s -> sb.Append('"').Append(escape s).Append('"') |> ignore
            | DInt i -> sb.Append(string i) |> ignore
            | DI64 v -> sb.Append('"').Append(int64Text v).Append('"') |> ignore
            | DReal f -> sb.Append(real f) |> ignore
            | DRec fields -> members fields
            | DMap entries -> entries |> List.sortWith (fun (a, _) (b, _) -> ordinal a b) |> members
            | DList items ->
                sb.Append '[' |> ignore

                items
                |> List.iteri (fun i item ->
                    if i > 0 then
                        sb.Append ',' |> ignore

                    go item)

                sb.Append ']' |> ignore

        and members (fields: (string * Doc) list) =
            sb.Append '{' |> ignore

            fields
            |> List.iteri (fun i (k, v) ->
                if i > 0 then
                    sb.Append ',' |> ignore

                sb.Append('"').Append(escape k).Append("\":") |> ignore
                go v)

            sb.Append '}' |> ignore

        go d
        sb.ToString()

    // ---- the reader ----------------------------------------------------------

    exception private ParseFailure of string

    /// Parse a JSON document (RFC 8259). Small and total: it holds `null`, preserves
    /// member order and duplicate members, and keeps every number's token.
    let parse (input: string) : Result<JsonValue, string> =
        let n = input.Length
        let mutable i = 0

        let fail (msg: string) : 'a =
            raise (ParseFailure(msg + " at offset " + string i))

        let peek () = if i < n then input[i] else '\000'

        let skipWs () =
            while i < n
                  && (input[i] = ' ' || input[i] = '\t' || input[i] = '\n' || input[i] = '\r') do
                i <- i + 1

        let expect (c: char) =
            skipWs ()

            if i < n && input[i] = c then
                i <- i + 1
            else
                fail ("expected '" + string c + "'")

        let hexDigit (c: char) =
            if c >= '0' && c <= '9' then int c - int '0'
            elif c >= 'a' && c <= 'f' then int c - int 'a' + 10
            elif c >= 'A' && c <= 'F' then int c - int 'A' + 10
            else fail "bad hex digit"

        // The string reader copies each clean run (everything up to the next `"` or `\`)
        // in one append. A raw control character inside a string is accepted, as it
        // always has been; refusals keep their messages and their offsets.
        let parseString () =
            expect '"'
            let start = i
            let close = indexOfAny stringStopSet input i

            if close >= 0 && input[close] = '"' then
                // No escape: the whole string is one run.
                i <- close + 1
                input.Substring(start, close - start)
            else
                let sb = System.Text.StringBuilder()
                let mutable finished = false
                let mutable stop = close

                while not finished do
                    let runEnd = if stop < 0 then n else stop
                    sb.Append(input, i, runEnd - i) |> ignore
                    i <- runEnd

                    if i >= n then
                        fail "unterminated string"

                    i <- i + 1

                    if input[i - 1] = '"' then
                        finished <- true
                    else
                        if i >= n then
                            fail "unterminated escape"

                        let e = input[i]
                        i <- i + 1

                        match e with
                        | '"' -> sb.Append '"' |> ignore
                        | '\\' -> sb.Append '\\' |> ignore
                        | '/' -> sb.Append '/' |> ignore
                        | 'b' -> sb.Append '\b' |> ignore
                        | 'f' -> sb.Append '\f' |> ignore
                        | 'n' -> sb.Append '\n' |> ignore
                        | 'r' -> sb.Append '\r' |> ignore
                        | 't' -> sb.Append '\t' |> ignore
                        | 'u' ->
                            if i + 4 > n then
                                fail "truncated \\u escape"

                            let code =
                                (hexDigit input[i] <<< 12)
                                + (hexDigit input[i + 1] <<< 8)
                                + (hexDigit input[i + 2] <<< 4)
                                + hexDigit input[i + 3]

                            i <- i + 4
                            sb.Append(char code) |> ignore
                        | other -> fail ("bad escape '\\" + string other + "'")

                        stop <- indexOfAny stringStopSet input i

                sb.ToString()

        let parseNumber () =
            let start = i

            if peek () = '-' then
                i <- i + 1

            while i < n && input[i] >= '0' && input[i] <= '9' do
                i <- i + 1

            if peek () = '.' then
                i <- i + 1

                while i < n && input[i] >= '0' && input[i] <= '9' do
                    i <- i + 1

            if peek () = 'e' || peek () = 'E' then
                i <- i + 1

                if peek () = '+' || peek () = '-' then
                    i <- i + 1

                while i < n && input[i] >= '0' && input[i] <= '9' do
                    i <- i + 1

            let token = input.Substring(start, i - start)

            match
                System.Double.TryParse(
                    token,
                    System.Globalization.NumberStyles.Float,
                    System.Globalization.CultureInfo.InvariantCulture
                )
            with
            | true, v when not (System.Double.IsNaN v || System.Double.IsInfinity v) -> JsonNumber(v, token)
            | _ -> fail ("malformed number '" + token + "'")

        let rec parseValue () =
            skipWs ()

            if i >= n then
                fail "unexpected end of input"

            match input[i] with
            | '"' -> JsonString(parseString ())
            | '{' -> parseObject ()
            | '[' -> parseArray ()
            | 't' -> literal "true" (JsonBool true)
            | 'f' -> literal "false" (JsonBool false)
            | 'n' -> literal "null" JsonNull
            | c when c = '-' || (c >= '0' && c <= '9') -> parseNumber ()
            | c -> fail ("unexpected character '" + string c + "'")

        and literal (text: string) (v: JsonValue) =
            if i + text.Length <= n && input.Substring(i, text.Length) = text then
                i <- i + text.Length
                v
            else
                fail ("expected '" + text + "'")

        and parseObject () =
            expect '{'
            let fields = ResizeArray<string * JsonValue>()
            skipWs ()

            if peek () = '}' then
                i <- i + 1
            else
                let mutable go = true

                while go do
                    skipWs ()
                    let key = parseString ()
                    expect ':'
                    let v = parseValue ()
                    fields.Add(key, v)
                    skipWs ()

                    if peek () = ',' then
                        i <- i + 1
                    else
                        expect '}'
                        go <- false

            JsonObject(List.ofSeq fields)

        and parseArray () =
            expect '['
            let items = ResizeArray<JsonValue>()
            skipWs ()

            if peek () = ']' then
                i <- i + 1
            else
                let mutable go = true

                while go do
                    items.Add(parseValue ())
                    skipWs ()

                    if peek () = ',' then
                        i <- i + 1
                    else
                        expect ']'
                        go <- false

            JsonArray(List.ofSeq items)

        try
            let v = parseValue ()
            skipWs ()

            if i <> n then
                Error("trailing content after the document at offset " + string i)
            else
                Ok v
        with ParseFailure msg ->
            Error msg

// ---- §4.1 content addresses -------------------------------------------------

/// Content addressing (§4.1): the string `"{algorithm}:{lowercase hex}"`.
///
/// The algorithm is named INSIDE the value so that a future digest change is a
/// visible discontinuity rather than a silent one.
[<RequireQualifiedAccess>]
module Digest =

    let private hex (bytes: byte[]) : string =
        bytes |> Array.map (fun b -> b.ToString "x2") |> String.concat ""

    /// SHA-256 over the UTF-8 bytes of `text`, as a content address.
    let sha256 (text: string) : string =
        let utf8 = System.Text.Encoding.UTF8.GetBytes text
        "sha256:" + hex (System.Security.Cryptography.SHA256.HashData utf8)

    /// The bare lowercase hex digest, with no algorithm prefix — for comparing a
    /// corpus file against a manifest that records digests that way. NOT a content
    /// address, deliberately, so the two can never be confused for one another.
    let sha256Hex (text: string) : string =
        hex (System.Security.Cryptography.SHA256.HashData(System.Text.Encoding.UTF8.GetBytes text))

// ---- §4.5 the minting canonicalisation --------------------------------------

/// The minting canonicalisation `canonical-json-sha256-v1` (§4.5).
///
/// §4.2 requires that two submitters which agree on a specification mint the same
/// `specHash` for it, and FORBIDS anyone downstream from checking that they did. This
/// module is the rule that makes the requirement mean something: agreement has to
/// come from a shared rule, because nothing at the seam is permitted to verify it.
///
/// The rule: parse the rendering; re-serialise it with every object's members ordered
/// ordinally ascending (recursively — a specification's interior is not a versioned
/// record with a published field order, so there is nothing to preserve and sorting is
/// precisely what makes two authoring orders mint one hash), arrays left in their
/// order (an array's order is data), numbers by §3.1 rule 9, strings by rule 6; then
/// SHA-256 those bytes as a content address.
///
/// **The wire payload is not these bytes.** The canonical form is an intermediate that
/// exists only to be hashed; the payload rides exactly as rendered. So two payloads
/// differing only in member order or whitespace differ on the wire and carry the same
/// hash — which is the property, stated the other way round.
[<RequireQualifiedAccess>]
module Minting =

    /// The identifier a submitter declares when it mints this way (§7.4).
    let algorithmId = "canonical-json-sha256-v1"

    /// A rendering the rule declines rather than resolving arbitrarily. Each case is
    /// outside the algorithm's DOMAIN — a submitter whose renderings can carry one of
    /// these registers its own algorithm identifier, which is an ordinary use of an
    /// open registry rather than a failure.
    type MintRefusal =
        /// Not a JSON document at all.
        | NotJson of detail: string
        /// Duplicate member names within one object. RFC 8259 permits them and no
        /// ordering makes them deterministic, so the only safe rule is to refuse.
        | DuplicateMembers of path: string * name: string
        /// A number that is not exactly representable as an IEEE-754 binary64.
        /// Rounding it silently collides two different specifications onto one
        /// identity; the shape the rule requires instead is a string.
        | NumberNotRepresentable of path: string * token: string
        /// An unpaired surrogate — no UTF-8 encoding, so no bytes to hash.
        | IllFormedUnicode of path: string

    let describeRefusal (r: MintRefusal) : string =
        match r with
        | NotJson detail -> "not a JSON rendering: " + detail
        | DuplicateMembers(path, name) ->
            "duplicate member '"
            + name
            + "' at '"
            + path
            + "' — no ordering of duplicates is deterministic"
        | NumberNotRepresentable(path, token) ->
            "the number '"
            + token
            + "' at '"
            + path
            + "' is not exactly representable as a binary64; render it as a string (§4.5)"
        | IllFormedUnicode path ->
            "ill-formed Unicode at '"
            + path
            + "' — an unpaired surrogate has no bytes to hash"

    /// Is this integer token exactly representable as a double?
    ///
    /// Judged on the TOKEN rather than on the parsed value: 2^53 + 1 rounds to 2^53, so
    /// a range check on the double would wrongly accept exactly the case that motivates
    /// the rule. JSON forbids leading zeros, so for equal length a lexical comparison of
    /// the digits IS the numeric order.
    let private integerTokenIsExact (token: string) : bool =
        let digits = if token.StartsWith "-" then token.Substring 1 else token

        digits.Length < 16
        || (digits.Length = 16
            && System.String.CompareOrdinal(digits, "9007199254740992") <= 0)

    /// Walk the value, refusing anything outside the rule's domain. The first refusal
    /// wins, and it names its path — a submitter debugging a hash disagreement needs to
    /// know WHERE, not only that.
    let rec private firstDomainRefusal (path: string) (v: JsonValue) : MintRefusal option =
        let at (segment: string) = path + "/" + segment

        match v with
        | JsonNull
        | JsonBool _ -> None
        | JsonString s ->
            if Canonical.isWellFormedUnicode s then
                None
            else
                Some(IllFormedUnicode path)
        | JsonNumber(_, token) ->
            let isInteger = not (token.Contains "." || token.Contains "e" || token.Contains "E")

            if isInteger && not (integerTokenIsExact token) then
                Some(NumberNotRepresentable(path, token))
            else
                None
        | JsonArray items ->
            items
            |> List.mapi (fun i item -> i, item)
            |> List.tryPick (fun (i, item) -> firstDomainRefusal (at (string i)) item)
        | JsonObject fields ->
            let duplicate =
                fields
                |> List.map fst
                |> List.countBy id
                |> List.tryFind (fun (_, count) -> count > 1)
                |> Option.map (fun (name, _) -> DuplicateMembers((if path = "" then "/" else path), name))

            let illFormedKey =
                fields
                |> List.map fst
                |> List.tryFind (Canonical.isWellFormedUnicode >> not)
                |> Option.map (fun _ -> IllFormedUnicode(if path = "" then "/" else path))

            match duplicate, illFormedKey with
            | Some d, _ -> Some d
            | _, Some k -> Some k
            | None, None -> fields |> List.tryPick (fun (k, child) -> firstDomainRefusal (at k) child)

    /// Serialise a value under the rule: every object sorted, arrays untouched.
    ///
    /// Keys sort by UTF-16 CODE UNIT (`CompareOrdinal`) — an implementation sorting by
    /// code point puts an astral-plane key after U+FFFD instead of before it and mints
    /// a different hash for the same specification.
    let rec private canonicalOf (v: JsonValue) : string =
        match v with
        | JsonNull -> "null"
        | JsonBool b -> (if b then "true" else "false")
        | JsonString s -> "\"" + Canonical.escape s + "\""
        | JsonNumber(value, _) -> Canonical.real value
        | JsonArray items -> "[" + (items |> List.map canonicalOf |> String.concat ",") + "]"
        | JsonObject fields ->
            "{"
            + (fields
               |> List.sortWith (fun (a, _) (b, _) -> System.String.CompareOrdinal(a, b))
               |> List.map (fun (k, child) -> "\"" + Canonical.escape k + "\":" + canonicalOf child)
               |> String.concat ",")
            + "}"

    /// The canonical bytes of a rendering — the intermediate, exposed because a
    /// submitter debugging a hash disagreement needs to see it. The corpus pins it for
    /// exactly that reason.
    let canonicalise (rendered: string) : Result<string, MintRefusal> =
        match Canonical.parse rendered with
        | Error detail -> Error(NotJson detail)
        | Ok parsed ->
            match firstDomainRefusal "" parsed with
            | Some refusal -> Error refusal
            | None -> Ok(canonicalOf parsed)

    /// Mint a spec hash from a rendering.
    let mint (rendered: string) : Result<string, MintRefusal> =
        canonicalise rendered |> Result.map Digest.sha256
