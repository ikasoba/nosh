module Nosh.lang.ParserUtil

open FParsec

let separateBy p d =
  let resParser, resParserRef = createParserForwardedToRef<'a list, 'u>()

  resParserRef := attempt (p .>> d .>>. resParser |>> (fun (x, y) -> [x] @ y)) <|> attempt (p |>> (fun x -> [x])) <|> (skipString "" |>> (fun _ -> []))

  resParser