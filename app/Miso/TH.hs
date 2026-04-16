module Miso.TH (makeSumTypeJSONInstances) where

import Language.Haskell.TH

{- | Generates `Miso.JSON.FromJSON` and `Miso.JSON.ToJSON` instances for a given sum type name.
Example usage:

> {\-# LANGUAGE DeriveAnyClass #-\}
> {\-# LANGUAGE TemplateHaskell #-\}
>
> import GHC.Generics (Generic)
> import Miso (MisoString)
> import Miso.JSON qualified -- must be imported qualified for `makeSumTypeJSONInstances` to work
> import Miso.TH (makeSumTypeJSONInstances)
>
> data MyTestRecord = MyTestRecord
>     { myInt :: Int
>     , myString :: MisoString
>     }
>     deriving (Show, Eq, Generic, Miso.JSON.FromJSON, Miso.JSON.ToJSON)
>
> data Foobar = Foo | Bar Int
>     deriving (Show, Eq, Generic)
> $(Miso.TH.makeSumTypeJSONInstances ''Foobar)
>
> data TestSumType
>     = ConstructorA Int
>     | ConstructorB MisoString Int Int
>     | ConstructorC {myInt :: Int, myString :: MisoString}
>     | ConstructorD Int MyTestRecord
>     | ConstructorE Foobar
>     deriving (Show, Eq, Generic)
> $(Miso.TH.makeSumTypeJSONInstances ''TestSumType)

Note due to TH restrictions MyTestRecord and Foobar must be declared before TestSumType.
-}
makeSumTypeJSONInstances :: Name -> Q [Dec]
makeSumTypeJSONInstances sumTypeName = do
    info <- reify sumTypeName
    dataConstructors <- case info of
        TyConI (DataD _ _ _ _ constructors _) -> pure constructors
        _ -> fail "makeSumTypeJSONInstances: Expected a data constructor"
    fromJSONInstanceDeclaration <-
        mkFromJSONInstance sumTypeName dataConstructors
    toJSONInstanceDeclaration <-
        mkToJSONInstance sumTypeName dataConstructors
    pure [fromJSONInstanceDeclaration, toJSONInstanceDeclaration]

-- | instance FromJSON SumTypeName where ...
mkFromJSONInstance :: Name -> [Con] -> Q Dec
mkFromJSONInstance sumTypeName dataConstructors = do
    objName <- newName "obj"
    tagName <- newName "tag"
    let sumTypeNameStr = nameBase sumTypeName
        -- e.g.
        -- > "ConstructorA" -> do
        -- >     param1 <- obj .: "_param1"
        -- >     pure (ConstructorA param1)
        caseBranches = fmap (mkFromJSONCaseBranch objName) dataConstructors

        -- e.g. `_ -> fail "Invalid SumTypeName tag"`
        defaultCaseBranch =
            match
                wildP
                ( normalB
                    ( appE
                        (varE (mkName "fail"))
                        (litE (stringL ("Invalid " <> sumTypeNameStr <> " tag")))
                    )
                )
                []

        -- e.g.
        -- > case tag :: MisoString of
        -- >    "ConstructorA" -> ...
        -- >    ...
        -- >   _ -> fail "Invalid SumTypeName tag"
        caseTagOfExpression =
            caseE
                (sigE (varE tagName) (conT (mkName "MisoString")))
                (caseBranches <> [defaultCaseBranch])

        -- tag <- obj .: "_tag"
        tagBind =
            bindS
                (varP tagName)
                ( appE
                    (appE (varE (mkName "Miso.JSON..:")) (varE objName))
                    (litE (stringL "_tag"))
                )

        -- do { tag <- ...; case tag :: MisoString of ... }
        doBlock =
            doE
                [ tagBind
                , noBindS caseTagOfExpression
                ]

        -- \obj -> do ...
        withObjectLambda = lamE [varP objName] doBlock

        -- Miso.JSON.withObject "TypeName" \obj -> do ...
        parseJSONBody =
            (varE (mkName "Miso.JSON.withObject") `appE` litE (stringL sumTypeNameStr))
                `appE` withObjectLambda

    instanceD
        (cxt [])
        (appT (conT (mkName "Miso.JSON.FromJSON")) (conT sumTypeName))
        [funD (mkName "parseJSON") [clause [] (normalB parseJSONBody) []]]

{- | e.g.
> "ConstructorA" -> do
>     param1 <- obj .: "_param1"
>     pure (ConstructorA param1)
-}
mkFromJSONCaseBranch :: Name -> Con -> Q Match
mkFromJSONCaseBranch objName dataConstructor = do
    let (constructorName, paramJSONKeys) = constructorNameAndParamJSONKeys dataConstructor
        constructorNameStr = nameBase constructorName
    paramNames <- mapM (\i -> newName ("param" <> show i)) [1 .. length paramJSONKeys]
    -- paramN <- obj .: "jsonKey"
    let mkBind key pName =
            bindS
                (varP pName)
                ( appE
                    (appE (varE (mkName "Miso.JSON..:")) (varE objName))
                    (litE (stringL key))
                )
        -- pure (Constructor param1 param2 ...)
        pureExp =
            appE
                (varE (mkName "pure"))
                (foldl appE (conE constructorName) (fmap varE paramNames))

        doBlockStatements = zipWith mkBind paramJSONKeys paramNames <> [noBindS pureExp]

    match
        (litP (stringL constructorNameStr))
        (normalB (doE doBlockStatements))
        []

-- | e.g. instance ToJSON SumTypeName where ...
mkToJSONInstance :: Name -> [Con] -> Q Dec
mkToJSONInstance name dataConstructors = do
    valName <- newName "val"
    -- e.g.
    -- > ConstructorA ->
    -- >     Miso.JSON.object
    -- >        [ "_tag" .= "ConstructorA"
    -- >        , "_param1" .= 0
    -- >        ]
    let caseBranches = fmap mkToJSONCaseBranch dataConstructors
        -- `\val -> case val of { ... }`
        toJSONBody = lamE [varP valName] (caseE (varE valName) caseBranches)

    instanceD
        (cxt [])
        (appT (conT (mkName "Miso.JSON.ToJSON")) (conT name))
        [funD (mkName "toJSON") [clause [] (normalB toJSONBody) []]]

{- | e.g.
> ConstructorA ->
>     Miso.JSON.object
>        [ "_tag" .= "ConstructorA"
>        , "_param1" .= 0
>        ]
-}
mkToJSONCaseBranch :: Con -> Q Match
mkToJSONCaseBranch dataConstructor = do
    let (constructorName, paramJSONKeys) = constructorNameAndParamJSONKeys dataConstructor
        constructorNameStr = nameBase constructorName
    paramNames <- mapM (\i -> newName ("param" <> show i)) [1 .. length paramJSONKeys]
    -- e.g. ConstructorB param1 param2 ->
    let casePattern = conP constructorName (fmap varP paramNames)

        -- "_tag" .= ("Constructor" :: MisoString)
        tagPair =
            appE
                (appE (varE (mkName "Miso.JSON..=")) (litE (stringL "_tag")))
                (sigE (litE (stringL constructorNameStr)) (conT (mkName "MisoString")))

        -- "jsonKey" .= paramN
        mkParamPair key pName =
            appE
                (appE (varE (mkName "Miso.JSON..=")) (litE (stringL key)))
                (varE pName)
        pairs = tagPair : zipWith mkParamPair paramJSONKeys paramNames

        -- Miso.JSON.object [ ... ]
        body = appE (varE (mkName "Miso.JSON.object")) (listE pairs)

    match casePattern (normalB body) []

constructorNameAndParamJSONKeys :: Con -> (Name, [String])
constructorNameAndParamJSONKeys dataConstructor =
    case dataConstructor of
        -- e.g. `ConstructorB "Hello" 1 2`
        NormalC name fields ->
            ( name
            , fmap (\i -> "_param" <> show i) [1 .. length fields]
            )
        -- e.g. `ConstructorC {myInt = 42, myString = "dummy"}`
        RecC name fields ->
            ( name
            , fmap (\(fieldName, _, _) -> nameBase fieldName) fields
            )
        _ ->
            error
                ( show dataConstructor
                    <> " constructor variant not supported by makeSumTypeJSONInstances"
                )
