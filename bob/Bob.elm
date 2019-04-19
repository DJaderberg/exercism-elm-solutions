module Bob exposing (hey)


hey : String -> String
hey remark =
    let
        trimmed =
            String.trim remark

        isSilence =
            String.isEmpty trimmed

        isShouted =
            let
                alphas =
                    String.filter Char.isAlpha trimmed
            in
            not (String.isEmpty alphas) && String.all Char.isUpper alphas

        isQuestion =
            String.endsWith "?" trimmed
    in
    if isSilence then
        "Fine. Be that way!"

    else if isShouted && isQuestion then
        "Calm down, I know what I'm doing!"

    else if isShouted then
        "Whoa, chill out!"

    else if isQuestion then
        "Sure."

    else
        "Whatever."
