port module Main exposing (..)

import Browser
import Html exposing (Html, div, textarea, button, text, table, th, tr, td,span,input,br)
import Html.Attributes exposing (class, placeholder, value,style, min ,step,type_)
import Html.Events exposing (onClick, onInput)
import Regex exposing (fromString, find)
import Html.Attributes as Attr
import Time exposing (Posix)
import Html.Events exposing (on)
import Json.Decode as Decode
import Html exposing (p)
import Html.Attributes exposing (id)
import Html.Attributes exposing (classList)
import Html exposing (h2)
import Html exposing (ul)
import Html exposing (li)
import Html exposing (thead)
import Html exposing (tbody)
import Html.Attributes exposing (spellcheck)
import Html.Attributes exposing (attribute)

port importCode  : () -> Cmd msg
port exportCode : String -> Cmd msg
port receiveCodeFromFile : (String -> msg) -> Sub msg

port scrollToRowInParsedTable : String -> Cmd msg  

port scrollToRowInRegisterTable : String -> Cmd msg
port scrollOverlayTo : Float -> Cmd msg
port updateCodeWithComments : (String -> msg) -> Sub msg
port saveToLocalStorage : String -> Cmd msg
-- MODEL

type alias Model =
    { code : String
    , registers : List Register
    , commands : List String
    , currentStep : Int
    , highlightedRegister : Maybe Int
    , executedCommands : List String
    , sliderValue : Float
    , isRunning : Bool
    , scrollPosition : Float
    , errorMessage : Maybe String
    , showHelpModal : Bool
    , stepsCount : Int
    , maxSteps : Int
    }

type alias Register =
    { number : Int
    , value : Int
    }


initialModel : String -> Model
initialModel savedCode =

    { code = savedCode
    , registers = List.range 0 100 |> List.map (\n -> { number = n, value = 0 })
    , commands = []
    , currentStep = -1
    , highlightedRegister = Nothing
    , executedCommands = []
    , sliderValue = 5 
    , isRunning = False
    , scrollPosition = 0
    , errorMessage = Nothing
    , showHelpModal = False
    , stepsCount = 0
    , maxSteps = 20000
    }


-- UPDATE

type Msg
    = CompileCode
    | ExportCode
    | ImportCode
    | CodeReceivedFromFile String
    | Step
    | SliderChanged String
    | StartTimer
    | StopExecution
    | ExecuteAction
    | StopTimer
    | SyncScroll Float
    | CloseError
    | ToggleHelpModal
    | IgnoredSliderChange String
    | CodeWithComments String 
    | UpdateCodeAndSave String
    
    

subscriptions : Model -> Sub Msg
subscriptions model =
    let
        interval =
            case round model.sliderValue of 
                1 -> 1000
                2 -> 700
                3 -> 500
                4 -> 300
                _ -> 1000 
    in
    Sub.batch
        [ updateCodeWithComments CodeWithComments,
            receiveCodeFromFile CodeReceivedFromFile 
        , if model.isRunning && model.currentStep < List.length model.executedCommands then
            Time.every (toFloat interval) (always Step)  
          else
            Sub.none
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CompileCode ->
            let
                newModel = compileCode model
                shouldRun = newModel.sliderValue < 5 
            in
            ( { newModel | isRunning = shouldRun , stepsCount = 0}, Cmd.none ) 

        ExportCode ->
            (model, exportCode model.code)

        ImportCode ->
            ( model, importCode() )

        CodeReceivedFromFile importedCode ->
            ( { model | code = importedCode }, Cmd.none )    

        Step ->
            let
                initializedModel =
                    if model.currentStep == -1 then
                        let
                            modelWithClearedExecuted = { model | executedCommands = [] }
                            compiledModel = compileCode modelWithClearedExecuted 
                        in
                        { compiledModel | commands = [] ,stepsCount = 0 } 
                    else
                        model

                resetRegisters =
                    if initializedModel.currentStep == -1 then
                        List.map (\reg -> { reg | value = 0 }) initializedModel.registers
                    else
                        initializedModel.registers
   
                parsedCommands = initializedModel.executedCommands

               
                updatedModel =
                    { initializedModel
                        | commands = parsedCommands
                        , registers = resetRegisters
                        
                    } 
            in

            if updatedModel.currentStep < List.length updatedModel.commands then
                let
                    maybeCommand =
                        List.head (List.drop updatedModel.currentStep updatedModel.commands)
                in
                case maybeCommand of
                    Nothing ->
                       
                         ( { updatedModel | currentStep = 0 }, Cmd.none )

                    Just cmd ->
                        if updatedModel.currentStep == -1 then
                           
                            ( { updatedModel | currentStep = 0 }, Cmd.none )
                        else
                            let
                                
                                ( updatedRegs, changedReg ) =
                                    processCommandStep cmd updatedModel.registers

                                finalModel =
                                    if updatedModel.currentStep + 1 == List.length updatedModel.commands then
                                        { updatedModel
                                            | registers = updatedRegs
                                            , currentStep = -1 
                                            , isRunning = False 
                                            , highlightedRegister = Nothing
                                            , executedCommands = [] 
                                        }
                                    else
                                        { updatedModel
                                            | registers = updatedRegs
                                            , currentStep = updatedModel.currentStep + 1
                                            , highlightedRegister = changedReg
                                        }
                                scrollCmd = scrollToRowInParsedTable ("command-" ++ String.fromInt model.currentStep) 
                                scrollRegCmd =
                                    case changedReg of
                                        Just regNum -> scrollToRowInRegisterTable ("register-" ++ String.fromInt regNum) 
                                        Nothing -> Cmd.none  
                            in
                            ( finalModel, Cmd.batch [ scrollCmd , scrollRegCmd])
            else 
                (  model , Cmd.none )

        SliderChanged newValue ->
            ( { model | sliderValue = String.toFloat newValue |> Maybe.withDefault model.sliderValue }
            , Cmd.none
            )
        StartTimer ->
            ( { model | isRunning = True }, Cmd.none ) 

        StopExecution ->
            ( { model 
                | currentStep = -1
                , highlightedRegister = Nothing
                , isRunning = False
              }
            , Cmd.none
            )

        ExecuteAction ->
            let
                resetModel = { model | executedCommands = [], currentStep = -1 } 
            in
            if resetModel.sliderValue < 5 then
                let
                    updatedModel = { resetModel | isRunning = True } 
                in
                update Step updatedModel 
            else
                update CompileCode resetModel   
        StopTimer ->
            ( { model | isRunning = False, currentStep = -1 ,executedCommands = []}, Cmd.none )
        
        CloseError ->
            ({ model | errorMessage = Nothing,executedCommands = [] }, Cmd.none)      

        SyncScroll scrollTop ->
            ( { model | scrollPosition = scrollTop }
            , scrollOverlayTo scrollTop
            )
        ToggleHelpModal ->
            ( { model | showHelpModal = not model.showHelpModal }, Cmd.none )   

        IgnoredSliderChange _->
            (model, Cmd.none)

        CodeWithComments newCode ->
            ({ model | code = newCode }, Cmd.none)    
            
        UpdateCodeAndSave newCode ->

            ( { model | code = newCode, commands = model.executedCommands }, saveToLocalStorage newCode )        

createErrorState : String -> Model -> Model
createErrorState errorMsg model =
    { model | errorMessage = Just errorMsg , executedCommands = [] }


compileCode : Model -> Model
compileCode model =

    case parseCommands model.code of
        Ok parsedCommands ->
            let
                resetModel =
                    { model 
                        | registers = List.map (\reg -> { reg | value = 0 }) model.registers
                        , errorMessage = Nothing
                    }

                finalModel =
                    List.foldl processCommand resetModel parsedCommands
            in
            { finalModel
                | commands = parsedCommands
                , currentStep = -1
                , highlightedRegister = Nothing
            }

        Err errorMessage ->

            
            { model | errorMessage = Just errorMessage }



parseCommands : String -> Result String (List String)
parseCommands code =
    let
        cleanedCode =
            removeComments code

        noSpacesCode =
            cleanedCode
                |> String.replace " " ""
                |> String.replace "\n" ""
    in
    parseMany noSpacesCode




parseMany : String -> Result String (List String)
parseMany input =
    case parseOne input of
        Ok ("", "") ->
            Ok []

        Ok (token, leftover) ->
            if leftover == "" then
                Ok [token]
            else
                if leftover == input then
                    Err ("Parser zaostal: leftover == input: " ++ leftover)
                else
                    case parseMany leftover of
                        Ok tokens -> Ok (token :: tokens)
                        Err err -> Err err

        Err err -> Err err




parseOne : String -> Result String (String, String)
parseOne str =
    case String.uncons str of
        Nothing ->
            Ok ("", "")

        Just (c, rest) ->
            case c of
                'a' ->
                    case parseNumberAfter 'a' rest of
                        Just (token, leftover) -> Ok (token, leftover)
                        Nothing -> Err "Očakávalo sa číslo po príkaze 'a', napr. a1 alebo a3."

                's' ->
                    case parseNumberAfter 's' rest of
                        Just (token, leftover) -> Ok (token, leftover)
                        Nothing -> Err "Očakávalo sa číslo po príkaze 's', napr. s2 alebo s5."

                '(' ->
                    case parseParenAndRegister rest of
                        Just (token, leftover) -> Ok (token, leftover)
                        Nothing -> Err "Slučka má neplatný formát. Skontroluj zátvorky a číslo registra na konci."

                _ ->
                    Err ("Neznámy príkaz: očakáva sa 'a', 's' alebo '(' .")


parseNumberAfter : Char -> String -> Maybe (String, String)
parseNumberAfter letter input =
    let
        (digits, leftover) =
            takeDigits input
    in
    if String.isEmpty digits then
        Nothing
    else
        Just ( String.fromChar letter ++ digits, leftover )

parseParenAndRegister : String -> Maybe (String, String)
parseParenAndRegister input =
    let
        (inside, leftover, ok) =
            parseInsideParen input 0 ""
    in
    if not ok then
        Nothing
    else
        let
            (digits, rest) =
                takeDigits leftover
        in
        case digits of
            "" ->
                Just ("(" ++ inside ++ ")", rest)

            _ ->
                Just ("(" ++ inside ++  ")" ++ digits, rest)


parseInsideParen : String -> Int -> String -> (String, String, Bool)
parseInsideParen input nesting acc =
    case String.uncons input of
        Nothing ->
            (acc, "", False)

        Just (ch, rest) ->
            case ch of
                '(' ->
                    parseInsideParen rest (nesting + 1) (acc ++ "(")

                ')' ->
                    if nesting == 0 then
                        (acc, rest, True)
                    else
                        parseInsideParen rest (nesting - 1) (acc ++ ")")

                _ ->
                    parseInsideParen rest nesting (acc ++ String.fromChar ch)


takeDigits : String -> (String, String)
takeDigits str =
    let
        (digitsList, leftoverList) =
            takeWhileList Char.isDigit (String.toList str)
    in
    ( String.fromList digitsList
    , String.fromList leftoverList
    )


takeWhileList : (a -> Bool) -> List a -> (List a, List a)
takeWhileList pred list =
    case list of
        [] ->
            ([], [])

        x :: xs ->
            if pred x then
                let
                    (ys, zs) =
                        takeWhileList pred xs
                in
                (x :: ys, zs)
            else
                ([], list)


processCommandStep : String -> List Register -> ( List Register, Maybe Int )
processCommandStep command registers =
    case String.uncons command of
        Just ( 'a', rest ) ->
            case String.toInt rest of
                Just n ->
                    let
                        updatedRegs = List.map (incrementRegister n) registers
                    in
                    ( updatedRegs, Just n )

                Nothing ->
                    ( registers, Nothing )

        Just ( 's', rest ) ->
            case String.toInt rest of
                Just n ->
                    let
                        updatedRegs = List.map (decrementRegister n) registers
                    in
                    ( updatedRegs, Just n )

                Nothing ->
                    ( registers, Nothing )

        Just ( '(', rest ) ->
            case String.split ")" rest of
                [ innerCommands, registerStr ] ->
                    case String.toInt registerStr of
                        Just n ->
                            ( registers, Just n )  

                        Nothing ->
                            ( registers, Nothing )

                _ ->
                    ( registers, Nothing )

        _ ->
            ( registers, Nothing )


processCommand : String -> Model -> Model
processCommand command model =
    let
        (updatedModel, shouldAddToExecuted) =
            case String.uncons command of
                Just ( 'a', rest ) ->
                    case String.toInt rest of
                        Just n ->
                            if List.any (\reg -> reg.number == n) model.registers then
                                ( { model | registers = List.map (incrementRegister n) model.registers }, True )
                            else
                                (createErrorState ("Register " ++ String.fromInt n ++ " neexistuje. Prikaz : " ++ command) model, False)

                        Nothing ->
                            (createErrorState "Očakávalo sa číslo po príkaze 'a', napr. a1 alebo a3." model, False)

                Just ( 's', rest ) ->
                    case String.toInt rest of
                        Just n ->
                            if List.any (\reg -> reg.number == n) model.registers then
                                ( { model | registers = List.map (decrementRegister n) model.registers }, True )
                            else
                                (createErrorState ("Register " ++ String.fromInt n ++ " neexistuje. Prikaz : " ++ command) model, False)

                        Nothing ->
                            (createErrorState "Očakávalo sa číslo po príkaze 's', napr. s1 alebo s3." model, False)

                Just ( '(', rest ) ->
                    case parseLoopCommand rest of
                        Just (innerCommands, regNum) ->
                            if List.any (\reg -> reg.number == regNum) model.registers then
                                (executeLoop innerCommands regNum model, False)
                            else
                                (createErrorState ("Register " ++ String.fromInt regNum ++ " neexistuje. Prikaz : " ++ command) model, False)

                        Nothing ->
                            (createErrorState "Neplatný formát slučky." model, False)

                _ -> (createErrorState "Neplatný príkaz processCommand." model, False)

        finalModel =
            { updatedModel
                | executedCommands =
                    if shouldAddToExecuted then updatedModel.executedCommands ++ [command]
                    else updatedModel.executedCommands
            }
    in
    finalModel


executeLoop : String -> Int -> Model -> Model
executeLoop innerCommands n model =
   

    if model.stepsCount >= model.maxSteps then
        { model | errorMessage = Just "Bol prekročený limit iterácií. Možný nekonečný cyklus." }

    else
        case getRegisterValue n model.registers of
            Just value ->
                if value > 0 then
                    case parseCommands innerCommands of
                        Ok parsedCommands ->
                            let
                                updatedModel =
                                    List.foldl processCommand model parsedCommands
                            in
                            executeLoop innerCommands n
                                { updatedModel
                                    | stepsCount = model.stepsCount + 1
                                }

                        Err error ->
                            { model | errorMessage = Just error }

                else
                    model

            Nothing ->
                { model | errorMessage = Just ("Регистр " ++ String.fromInt n ++ " не существует.") }



parseLoopCommand : String -> Maybe (String, Int)
parseLoopCommand input =
    let
        (inside, leftover, ok) =
            parseInsideParen input 0 ""
    in
    if not ok then
        Nothing
    else
        let
            (digits, leftover2) =
                takeDigits leftover
        in
        case String.toInt digits of
            Just regNum ->

                Just (inside, regNum)

            Nothing ->
                Nothing

getRegisterValue : Int -> List Register -> Maybe Int
getRegisterValue n registers =
    List.head <| List.map .value (List.filter (\reg -> reg.number == n) registers)


incrementRegister : Int -> Register -> Register
incrementRegister n reg =
    if reg.number == n then
        { reg | value = reg.value + 1 }
    else
        reg


decrementRegister : Int -> Register -> Register
decrementRegister n reg =
    if reg.number == n then
        { reg | value = max 0 (reg.value - 1) }  
    else
        reg


parseAndReset : Model -> Model
parseAndReset model =
    case parseCommands model.code of
        Ok parsedCommands ->
            let
                resetRegisters =
                    List.map (\reg -> { reg | value = 0 }) model.registers
            in
            { model
                | registers = resetRegisters
                , commands = parsedCommands
                , currentStep = -1
                , highlightedRegister = Nothing
                , errorMessage = Nothing
            }

        Err errorMessage ->
            { model | errorMessage = Just errorMessage }


runAllCommands : Model -> Model
runAllCommands model =
    let
        finalModel =
            List.foldl processCommand model model.commands 
    in
    { finalModel
        | currentStep = List.length model.commands
        , highlightedRegister = Nothing
    }

removeComments : String -> String
removeComments code =
    code
        |> String.lines
        |> List.map (\line -> 
            case String.split "//" line of
                firstPart :: _ -> String.trim firstPart 
                [] -> line
           )
        |> String.join "\n"  


highlightCode : String -> Html msg
highlightCode code =
    let
        splittedLines =
            String.split "\n" code

        parseLine : String -> ( String, String )
        parseLine line =
            case String.split "//" line of
                firstPart :: rest ->
                    let
                        comment =
                            case rest of
                                commentPart :: _ ->
                                    "//" ++ commentPart
                                [] ->
                                    ""
                    in
                    ( firstPart, comment )

                [] ->
                    ( line, "" )

        formattedLines =
            splittedLines
                |> List.map (\line ->
                    let
                        (codePart, comment) =
                            if String.isEmpty line then
                                ("", "")
                            else
                                parseLine line
                    in
                    if String.isEmpty codePart && String.isEmpty comment then
                        div [] [ br [] [] ]
                    else
                        let
                            maybePattern =
                                Regex.fromString "([as]\\d+)|(\\()|(\\))|(\\s+)|([^()\\s]+)"

                            foundMatches =
                                case maybePattern of
                                    Nothing ->
                                        []
                                    Just pattern ->
                                        Regex.find pattern codePart

                            commands =
                                List.map (\m -> m.match) foundMatches

                            formatCommand cmd =
                                case String.uncons cmd of
                                    Just ('a', _) ->
                                        span [ style "color" "green" ] [ text cmd ]

                                    Just ('s', _) ->
                                        span [ style "color" "red" ] [ text cmd ]

                                    _ ->
                                        text cmd

                            formattedCommands =
                                List.map formatCommand commands
                        in
                        div []
                            ( formattedCommands
                                ++ [ span [ style "color" "gray" ] [ text comment ] ]
                            )
                )
    in
    div [ class "highlight-container" ] formattedLines


-- VIEW

view : Model -> Html Msg
view model =

    div [ class "wrapper"] 
        [
          div [ class "header-container" ]
            [ div [ class "header-title" ] [ text "Abacus Machine" ]
            , viewHelpModal model.showHelpModal
            , div [ class "controls" ]
                [ button [ onClick ExecuteAction, class "control-button" ] [ text "Spustit" ]
                , button [ onClick StopExecution, class "control-button" ] [ text "Zastavit" ]
                , button [ onClick Step, class "control-button" ] [ text "Krok" ]
                , button [ onClick ToggleHelpModal, class "control-button" ] [ text "?" ]
                ]
                , viewSlider model.sliderValue SliderChanged
            , div [ class "import-export-buttons" ]
                [ button [ onClick ImportCode, class "import-button" ] [ text "Import code" ]
                , button [ onClick ExportCode, class "export-button" ] [ text "Export code" ]
                ]
            ]
        
        , div [ class "main-container" ]
            [ 
              div [ class "code-container" ]
                [  div 
                    [ class "highlight-overlay"]
                    [ highlightCode model.code ]
                ,textarea
                        [ id "code-area"
                        , placeholder "Miesto pre napísanie kodu..."
                        , value model.code
                        , onInput UpdateCodeAndSave
                        , class "code-area"
                        , on "scroll" (Decode.map SyncScroll scrollDecoder)
                        , spellcheck False
                        , attribute "autocorrect" "off"
                        , attribute "autocomplete" "off"
                        , attribute "autocapitalize" "off"
                        ]
                        []
                ]
            , div [ class "tables-wrapper" ][
                div [ class "register-container" ]
                [ viewExecutedCommands model
                ],
            div [ class "register-container" ]
                [ viewError model.errorMessage ,
                    table []
                    (tableHeader :: List.map (viewRegister model.highlightedRegister) model.registers )
                ]]
            ]
        ]
scrollDecoder : Decode.Decoder Float
scrollDecoder =
    Decode.at [ "target", "scrollTop" ] Decode.float
        
tableHeader : Html Msg
tableHeader =
    tr []
        [ th [] [ text "Číslo registru" ]
        , th [] [ text "Hodnota registru" ]
        ]

tableHeader2 : Html Msg
tableHeader2 =
    tr []
        [ th [] [ text "Index" ]
        , th [] [ text "Príkaz" ]
        ]        

viewSlider : Float -> (String -> Msg) -> Html Msg
viewSlider value msg =
    let
        speedLabels =
            [ "1", "2", "3", "4", "5" ]
    in
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "align-items" "center"
        , style "width" "150px"
        ]
        [ div
            [ style "margin-bottom" "5px"
            , style "font-size" "20px"
            , style "color" "black"
            ]
            [ text "Rýchlosť" ]
        , input
            [ type_ "range"
            , Attr.min "1"
            , Attr.max "5"
            , step "1"
            , Attr.value (String.fromFloat value)
            , onInput msg
            , style "width" "100%"
            , style "background" "#ddd"
            , style "height" "6px"
            , style "border-radius" "5px"
            , style "outline" "none"
            , style "appearance" "none"
            ]
            []
        , div
            [ style "display" "flex"
            , style "justify-content" "space-between"
            , style "width" "100%"
            , style "margin-top" "10px"
            , style "font-size" "15px"
            , style "color" "WHITE"
            ]
            (List.map (text >> (\label -> div [] [ label ])) speedLabels)
        ]

viewRegister : Maybe Int -> Register -> Html Msg
viewRegister maybeHighlighted reg =
    let
        isHighlighted =
            case maybeHighlighted of
                Just n -> n == reg.number
                Nothing -> False
    in
    tr
        [ id ("register-" ++ String.fromInt reg.number) 
        , classList [ ("highlighted", isHighlighted) ]
        ]
        [ td [] [ text (String.fromInt reg.number) ]
        , td [] [ text (String.fromInt reg.value) ]
        ]


viewError : Maybe String -> Html Msg
viewError errorMsg =
    case errorMsg of
        Just msg ->
            div [ class "error-message" ]
                [ p [] [ text msg ] 
                , div []  
                    [ button [ onClick CloseError, class "error-close-button" ] [ text "Zatvoriť" ] ]
                ]
        Nothing ->
            text ""
            
viewExecutedCommands : Model -> Html Msg
viewExecutedCommands model =
    if model.errorMessage == Nothing then
        table []
            (tableHeader2 :: List.indexedMap (viewExecutedCommand model.currentStep) model.executedCommands)
    else
        table []
            (tableHeader2 :: List.indexedMap (viewExecutedCommand model.currentStep)[])

viewExecutedCommand : Int -> Int -> String -> Html Msg
viewExecutedCommand currentStep index command =
    tr
        [ id ("command-" ++ String.fromInt index)  
        , classList [ ("highlighted", index == currentStep) ]
        ]
        [ td [] [ text (String.fromInt (index + 1)) ] 
        , td [] [ text command ] 
        ]

viewHelpModal : Bool -> Html Msg
viewHelpModal show =
    if show then
        div [ class "modal-backdrop" ]
            [ div [ class "modal" ]
                [ 
                div [ style "position" "relative" ]
                [ h2 [] [ text "Návod na používanie Abacus Machine:" ]
                , button
                    [ onClick ToggleHelpModal
                    , class "close-button"
                    , style "position" "absolute"
                    , style "top" "0"
                    , style "right" "0"
                    ]
                    [ text "X" ]
                ]
                , p [] [ text "Simulátor rozpoznáva nasledujúce príkazy:" ]
                , table [ class "help-table" ]
                    [ thead []
                        [ tr []
                            [ th [] [ text "Operácia" ]
                            , th [] [ text "Príkaz" ]
                            , th [] [ text "Popis" ]
                            ]
                        ]
                    , tbody []
                        [ tr []
                            [ td [] [ text "Inkrement registra" ]
                            , td [] [ text "an" ]
                            , td [] [ text "an, kde n je číslo registra, napr. a1, a2." ]
                            ]
                        , tr []
                            [ td [] [ text "Dekrement registra" ]
                            , td [] [ text "sn" ]
                            , td [] [ text "sn, kde n je číslo registra, napr. s3, s5" ]
                            ]
                        , tr []
                            [ td [] [ text "Zreťazenie" ]
                            , td [] [ text "M1M2...Mn" ]
                            , td [] [ text "operácie sa vykonávajú sekvenčne za sebou, napr. a1a1a1, a1a2a3." ]
                            ]
                        , tr []
                            [ td [] [ text "Cyklus" ]
                            , td [] [ text "(M)n" ]
                            , td [] [ text "vykoná sa stroj M opakovane, kým register n nebude mať hodnotu 0, napr. (a1)2" ]
                            ]
                        ]]
               , div [ style "display" "flex", style "align-items" "center", style "gap" "10px", style "color" "black",style "margin-bottom" "10px",style "margin-top" "10px" ]
                    [ button [ class "import-button" ] [ text "Import code" ]
                    , span [] [ text "– slúži na načítanie kódu zo súboru vo formáte .txt do priestoru na písanie kódu" ]
                    ]
                , div [ style "display" "flex", style "align-items" "center", style "gap" "10px", style "color" "black",style "margin-bottom" "10px" ]
                    [ button [ class "export-button" ] [ text "Export code" ]
                    , span [] [ text "– slúži na ukladanie kódu z priestoru na písanie kódu do súboru vo formáte .txt" ]
                    ] 
                , div [ style "display" "flex", style "align-items" "center", style "gap" "10px", style "color" "black",style "margin-bottom" "10px",style "margin-top" "10px"  ]
                    [ button [ class "control-button" ] [ text "Krok" ]
                    , span [] [ text "– slúži na krokové spúšťanie kódu, jedno stlačenie vykoná jeden príkaz" ]
                    ]   
                , div [ style "display" "flex", style "align-items" "center", style "gap" "10px", style "color" "black",style "margin-top" "10px" ]
                    [ button [ class "control-button" ] [ text "Spustit" ]
                    , span [] [ text "– slúži na spustenie kódu" ]
                    ]           
                , div
                    [ style "margin-top" "20px"
                    , style "color" "black"
                    ]
                    [ div
                        [ style "display" "flex"
                        , style "align-items" "center"
                        , style "gap" "10px"
                        ]
                        [ viewSlider2 5 IgnoredSliderChange
                        , span [] [ text "– slúži na nastavenie rýchlosti vykonávania programu." ]
                        ]
                    , ul [ style "margin-top" "10px", style "margin-left" "20px" ]
                        [ li [] [ text "Rýchlosť 1 – 1 príkaz za 1 sekundu" ]
                        , li [] [ text "Rýchlosť 2 – 1 príkaz za 0,7 sekundy" ]
                        , li [] [ text "Rýchlosť 3 – 1 príkaz za 0,5 sekundy" ]
                        , li [] [ text "Rýchlosť 4 – 1 príkaz za 0,3 sekundy" ]
                        , li [] [ text "Rýchlosť 5 – okamžité vykonanie všetkých príkazov" ]
                        ]
                    ]
                , div [style "display" "flex", style "align-items" "center", style "gap" "10px", style "color" "black",style "margin-top" "10px"]
                [span [] [ text "Klávesová skratka na komentovanie kódu:"
                    , Html.br [] []
                    , text "Windows/Linux: Ctrl + /"
                    , Html.br [] []
                    , text "macOS: Control + /",
                    instructionForCommenting
                    , br [] []
                    ,  pwaInfo
                    , br [] []
                    , text "Spätná väzba a otázky sú vítané : annasikalenko19@gmail.com"
                    ]
                ]
                ]
            ]
    else
        text ""

pwaInfo : Html msg
pwaInfo =
    div [style "color" "black"]
        [ text "Použitie simulátora ako PWA:"
        , br [] []
        , br [] []
        , text "Tento simulátor je možné používať ako PWA (Progressívna webová aplikácia), čo znamená, že si ho môžeš pridať na plochu ako samostatnú aplikáciu."
        , br [] []
        , text "Ako postupovať pri inštalácii v prehliadači Chrome:"
        , br [] []
        , text "• Klikni na tri bodky v pravom hornom rohu prehliadača."
        , br [] []
        , text "• Prejdi do sekcie Cast, save and share." 
        , br [] []
        , text "• Vyber možnosť Install page as app." 
        , br [] []
        , text "• Potvrď kliknutím na Install."
        , br [] []
        ] 
instructionForCommenting: Html msg
instructionForCommenting =
    div []
        [ text "Stlačením tejto skratky môžeš:"
        , br [] []
        , br [] []
        , text "• Pridať komentár (//) na začiatok riadku, v ktorom sa nachádza kurzor."
        , br [] []
        , text "• Pri označení viacerých riadkov — pridať komentár na začiatok každého z nich."
        , br [] []
        , text "• Opätovným stlačením tej istej skratky sa komentáre z týchto riadkov odstránia."
        ]
viewSlider2 : Float -> (String -> Msg) -> Html Msg
viewSlider2 value msg =
    let
        speedLabels =
            [ "1", "2", "3", "4", "5" ]
    in
    div
        [ style "display" "flex"
        , style "flex-direction" "column"
        , style "align-items" "center"
        , style "width" "150px"
        ]
        [ div
            [ style "margin-bottom" "5px"
            , style "font-size" "20px"
            , style "color" "black"
            ]
            [ text "Rýchlosť" ]
        , input
            [ type_ "range"
            , Attr.min "1"
            , Attr.max "5"
            , step "1"
            , Attr.value (String.fromFloat value)
            , onInput msg
            , style "width" "100%"
            , style "background" "#ddd"
            , style "height" "6px"
            , style "border-radius" "5px"
            , style "outline" "none"
            , style "appearance" "none"
            ]
            []
        , div
            [ style "display" "flex"
            , style "justify-content" "space-between"
            , style "width" "100%"
            , style "margin-top" "10px"
            , style "font-size" "15px"
            , style "color" "black"
            ]
            (List.map (text >> (\label -> div [] [ label ])) speedLabels)
        ]
-- MAIN

main : Program String Model Msg
main =
    Browser.element
        { init = \flags -> (initialModel flags, Cmd.none)
        , update = update
        , view = view
        , subscriptions = subscriptions
        }

