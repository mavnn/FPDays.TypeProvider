#load "Templates.fs"
open TypeSet.Templates

let templateTest = "Text {{ thing:int }} and other text"

let p = parseTemplate { ParserState.Empty with Rest = stringToChars templateTest } []
