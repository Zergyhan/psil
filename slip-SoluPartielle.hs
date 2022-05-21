-- TP-2  --- Implantation d'une sorte de Lisp          -*- coding: utf-8 -*-
{-# OPTIONS_GHC -Wall #-}

-- Ce fichier défini les fonctionalités suivantes:
-- - Analyseur lexical
-- - Analyseur syntaxique
-- - Évaluateur
-- - Pretty printer

---------------------------------------------------------------------------
-- Importations de librairies et définitions de fonctions auxiliaires    --
---------------------------------------------------------------------------

import Text.ParserCombinators.Parsec -- Libraire d'analyse syntaxique (et lexicale).
import Data.Char        -- Conversion de Chars de/vers Int et autres
-- import Numeric       -- Pour la fonction showInt
import System.IO        -- Pour stdout, hPutStr
-- import Data.Maybe    -- Pour isJust and fromJust

---------------------------------------------------------------------------
-- La représentation interne des expressions de notre language           --
---------------------------------------------------------------------------
data Sexp = Snil                        -- La liste vide
          | Scons Sexp Sexp             -- Une paire
          | Ssym String                 -- Un symbole
          | Snum Int                    -- Un entier
          -- Génère automatiquement un pretty-printer et une fonction de
          -- comparaison structurelle.
          deriving (Show, Eq)

-- Exemples:
-- (+ 2 3) == (+ . (2 . (3 . ())))
--         ==> Scons (Ssym "+")
--                   (Scons (Snum 2)
--                          (Scons (Snum 3) Snil))
--
-- (/ (* (- 68 32) 5) 9)
--     ==>
-- Scons (Ssym "/")
--       (Scons (Scons (Ssym "*")
--                     (Scons (Scons (Ssym "-")
--                                   (Scons (Snum 68)
--                                          (Scons (Snum 32) Snil)))
--                            (Scons (Snum 5) Snil)))
--              (Scons (Snum 9) Snil))

---------------------------------------------------------------------------
-- Analyseur lexical                                                     --
---------------------------------------------------------------------------

pChar :: Char -> Parser ()
pChar c = do { _ <- char c; return () }

-- Les commentaires commencent par un point-virgule et se terminent
-- à la fin de la ligne.
pComment :: Parser ()
pComment = do { pChar ';'; _ <- many (satisfy (\c -> not (c == '\n')));
                pChar '\n'; return ()
              }
-- N'importe quelle combinaison d'espaces et de commentaires est considérée
-- comme du blanc.
pSpaces :: Parser ()
pSpaces = do { _ <- many (do { _ <- space ; return () } <|> pComment); return () }

-- Un nombre entier est composé de chiffres.
integer     :: Parser Int
integer = do c <- digit
             integer' (digitToInt c)
          <|> do _ <- satisfy (\c -> (c == '-'))
                 n <- integer
                 return (- n)
    where integer' :: Int -> Parser Int
          integer' n = do c <- digit
                          integer' (10 * n + (digitToInt c))
                       <|> return n

-- Les symboles sont constitués de caractères alphanumériques et de signes
-- de ponctuations.
pSymchar :: Parser Char
pSymchar    = alphaNum <|> satisfy (\c -> c `elem` "!@$%^&*_+-=:|/?<>")
pSymbol :: Parser Sexp
pSymbol= do { s <- many1 (pSymchar);
              return (case parse integer "" s of
                        Right n -> Snum n
                        _ -> Ssym s)
            }

---------------------------------------------------------------------------
-- Analyseur syntaxique                                                  --
---------------------------------------------------------------------------

-- La notation "'E" est équivalente à "(quote E)"
pQuote :: Parser Sexp
pQuote = do { pChar '\''; pSpaces; e <- pSexp;
              return (Scons (Ssym "quote") (Scons e Snil)) }

-- Une liste est de la forme:  ( {e} [. e] )
pList :: Parser Sexp
pList  = do { pChar '('; pSpaces; pTail }
pTail :: Parser Sexp
pTail  = do { pChar ')'; return Snil }
     <|> do { pChar '.'; pSpaces; e <- pSexp; pSpaces;
              pChar ')' <|> error ("Missing ')' after: " ++ show e);
              return e }
     <|> do { e <- pSexp; pSpaces; es <- pTail; return (Scons e es) }

-- Accepte n'importe quel caractère: utilisé en cas d'erreur.
pAny :: Parser (Maybe Char)
pAny = do { c <- anyChar ; return (Just c) } <|> return Nothing

-- Une Sexp peut-être une liste, un symbol ou un entier.
pSexpTop :: Parser Sexp
pSexpTop = do { pSpaces;
                pList <|> pQuote <|> pSymbol
                <|> do { x <- pAny;
                         case x of
                           Nothing -> pzero
                           Just c -> error ("Unexpected char '" ++ [c] ++ "'")
                       }
              }

-- On distingue l'analyse syntaxique d'une Sexp principale de celle d'une
-- sous-Sexp: si l'analyse d'une sous-Sexp échoue à EOF, c'est une erreur de
-- syntaxe alors que si l'analyse de la Sexp principale échoue cela peut être
-- tout à fait normal.
pSexp :: Parser Sexp
pSexp = pSexpTop <|> error "Unexpected end of stream"

-- Une séquence de Sexps.
pSexps :: Parser [Sexp]
pSexps = do pSpaces
            many (do e <- pSexpTop
                     pSpaces
                     return e)

-- Déclare que notre analyseur syntaxique peut-être utilisé pour la fonction
-- générique "read".
instance Read Sexp where
    readsPrec _p s = case parse pSexp "" s of
                      Left _ -> []
                      Right e -> [(e,"")]

---------------------------------------------------------------------------
-- Sexp Pretty Printer                                                   --
---------------------------------------------------------------------------

showSexp' :: Sexp -> ShowS
showSexp' Snil = showString "()"
showSexp' (Snum n) = showsPrec 0 n
showSexp' (Ssym s) = showString s
showSexp' (Scons e1 e2) =
    let showTail Snil = showChar ')'
        showTail (Scons e1' e2') =
            showChar ' ' . showSexp' e1' . showTail e2'
        showTail e = showString " . " . showSexp' e . showChar ')'
    in showChar '(' . showSexp' e1 . showTail e2

-- On peut utiliser notre pretty-printer pour la fonction générique "show"
-- (utilisée par la boucle interactive de Hugs).  Mais avant de faire cela,
-- il faut enlever le "deriving Show" dans la déclaration de Sexp.
{-
instance Show Sexp where
    showsPrec p = showSexp'
-}

-- Pour lire et imprimer des Sexp plus facilement dans la boucle interactive
-- de Hugs:
readSexp :: String -> Sexp
readSexp = read
showSexp :: Sexp -> String
showSexp e = showSexp' e ""

---------------------------------------------------------------------------
-- Représentation intermédiaire L(ambda)exp(ression)                     --
---------------------------------------------------------------------------

type Var = String
type Tag = String
type Pat = Maybe (Tag, [Var])
data BindingType = Lexical | Dynamic
                   deriving (Show, Eq)
    
data Lexp = Lnum Int            -- Constante entière.
          | Lvar Var            -- Référence à une variable.
          | Lfn Var Lexp        -- Fonction anonyme prenant un argument.
          | Lpipe Lexp Lexp     -- Appel de fonction, avec un argument.
          | Lcons Tag [Lexp]    -- Constructeur de liste vide.
          | Lcase Lexp [(Pat, Lexp)] -- Expression conditionelle.
          | Llet BindingType Var Lexp Lexp -- Déclaration de variable locale
          deriving (Show, Eq)

-- Première passe simple qui analyse un Sexp et construit une Lexp équivalente.
s2l :: Sexp -> Lexp
s2l (Snum n) = Lnum n
s2l (Ssym s) = Lvar s
s2l (Scons (Ssym "lambda") (Scons args (Scons exp Snil))) =
    nestLambdas args exp
s2l (Scons (Ssym "dlet") (Scons decls (Scons exp Snil))) =
    nestDecls Dynamic decls exp
s2l (Scons (Ssym "slet") (Scons decls (Scons exp Snil))) =
    nestDecls Lexical decls exp
s2l (Scons (Ssym "if") (Scons cond (Scons thenExp (Scons elseExp Snil)))) =
    Lcase (s2l cond) [
        (Just ("true", []), s2l thenExp),
        (Just ("false", []), s2l elseExp)
    ]
s2l (Scons (Ssym "cons") (Scons (Ssym tag) items)) =
    Lcons tag (map s2l (scons2list items))
s2l (Scons (Ssym "case") (Scons expr branches)) =
    let getBranch (Scons pattern (Scons exp Snil)) =
            case pattern of
                Ssym "_" -> (Nothing, s2l exp)
                _ -> case scons2list pattern of
                        (Ssym tag) : variables ->
                            (Just (tag, map (\(Ssym var) -> var) variables), s2l exp)
                        _ -> error ("Malformed pattern: " ++ (showSexp pattern))
        getBranch b = error ("Malformed branch: " ++ (showSexp b))
    in Lcase (s2l expr) (map getBranch (scons2list branches))
s2l (Scons firstArg fnExp) =
    let buildPipe :: Lexp -> Sexp -> Lexp
        buildPipe arg (Scons fn Snil) = Lpipe arg (s2l fn)
        buildPipe arg (Scons fn rem) = buildPipe (Lpipe arg (s2l fn)) rem
    in buildPipe (s2l firstArg) fnExp
s2l se = error ("Malformed Sexp: " ++ (showSexp se))

scons2list :: Sexp -> [Sexp]
scons2list Snil = []
scons2list (Scons e rem) = e : (scons2list rem)
scons2list sexp = error ("Not a list: " ++ showSexp sexp)

nestLambdas :: Sexp -> Sexp -> Lexp
nestLambdas (Scons (Ssym arg) Snil) exp = Lfn arg (s2l exp)
nestLambdas (Scons (Ssym arg) rem) exp = Lfn arg (nestLambdas rem exp)
nestLambdas _ _ = error "Syntax error: wrong lambda definition"

nestDecls :: BindingType -> Sexp -> Sexp -> Lexp
nestDecls binding (Scons decl Snil) exp =
    let (varName, varLexp) = convertDef decl
    in Llet binding varName varLexp (s2l exp)
nestDecls binding (Scons decl rem) exp =
    let (varName, varLexp) = convertDef decl
    in Llet binding varName varLexp (nestDecls binding rem exp)

convertDef :: Sexp -> (Var, Lexp)
convertDef (Scons (Ssym name) (Scons exp Snil)) = (name, s2l exp)
convertDef (Scons (Scons (Ssym name) args) (Scons exp Snil)) =
    (name, nestLambdas args exp)


---------------------------------------------------------------------------
-- Représentation du contexte d'exécution                                --
---------------------------------------------------------------------------

type Arity = Int

-- Type des valeurs manipulée à l'exécution.
data Value = Vnum Int
           | Vcons Tag [Value]
           | Vfn (Env -> Value -> Value)

instance Show Value where
    showsPrec p (Vnum n) = showsPrec p n
    showsPrec p (Vcons tag vs) =
        let showTail [] = showChar ']'
            showTail (v : vs') =
                showChar ' ' . showsPrec p v . showTail vs'
        in showChar '[' . showString tag . showTail vs
    showsPrec _ (Vfn _)
        = showString ("<function>")

type Env = [(Var, Value)]

-- L'environnement initial qui contient les fonctions prédéfinies.
env0 :: Env
env0 = let false = Vcons "false" []
           true = Vcons "true" []
           mkbop (name, op) =
               (name, Vfn (\ _ (Vnum x)
                           -> Vfn (\ _ (Vnum y)
                                   -> Vnum (x `op` y))))
           mkcmp (name, op) =
               (name, Vfn (\ _ (Vnum x)
                           -> Vfn (\ _ (Vnum y)
                                   -> if x `op` y then true else false)))
       in [("false", false),
           ("true", true)]
          ++ map mkbop
              [("+", (+)),
               ("*", (*)),
               ("/", div),
               ("-", (-))]
          ++ map mkcmp
              [("<=", (<=)),
               ("<", (<)),
               (">=", (>=)),
               (">", (>)),
               ("=", (==))]

---------------------------------------------------------------------------
-- Évaluateur                                                            --
---------------------------------------------------------------------------
{-
data Lexp = Lnum Int            -- Constante entière.
          | Lvar Var            -- Référence à une variable.
          | Lfn Var Lexp        -- Fonction anonyme prenant un argument.
          | Lpipe Lexp Lexp     -- Appel de fonction, avec un argument.
          | Lcons Tag [Lexp]    -- Constructeur de liste vide.
          | Lcase Lexp [(Pat, Lexp)] -- Expression conditionelle.
          | Llet BindingType Var Lexp Lexp -- Déclaration de variable locale
          deriving (Show, Eq)
type Pat = Maybe (Tag, [Var])
          -}
eval :: Env -> Env -> Lexp -> Value
eval _senv _denv (Lnum n) = Vnum n
eval senv denv (Lvar var) =
    case lookup var (senv ++ denv) of
        Just value -> value
        _ -> error ("Undefined variable '" ++ var ++ "'")
eval senv _ (Lfn var lexp) =
    Vfn (\denv arg -> eval ((var,arg) : senv) denv lexp)
eval senv denv (Lpipe argLexp fnLexp) =
    let argValue = eval senv denv argLexp
        (Vfn fn) = eval senv denv fnLexp
    in fn denv argValue
eval senv denv (Lcons tag values) =
    Vcons tag (map (\v -> eval senv denv v) values)
eval senv denv (Lcase lexp pats) =
    let (Vcons expTag expVals) = eval senv denv lexp
        goThroughPatterns [] = error "Non-exhaustive pattern"
        goThroughPatterns ((pat, ret):rem) =
            case pat of
                Nothing -> eval senv denv ret
                (Just (tag, varList)) ->
                    if (length varList) == (length expVals) && tag == expTag
                    then eval (zip varList expVals ++ senv) denv ret
                    else goThroughPatterns rem
    in goThroughPatterns pats
eval senv denv (Llet binding var varLexp letLexp) =
    let varValue = eval senv denv varLexp
        (senv', denv') = case binding of
            Lexical -> ((var, varValue) : senv, denv)
            Dynamic -> (senv, (var, varValue) : denv)
    in eval senv' denv' letLexp

---------------------------------------------------------------------------
-- Toplevel                                                              --
---------------------------------------------------------------------------

evalSexp :: Sexp -> Value
evalSexp = eval env0 [] . s2l

-- Lit un fichier contenant plusieurs Sexps, les évalues l'une après
-- l'autre, et renvoie la liste des valeurs obtenues.
run :: FilePath -> IO ()
run filename =
    do s <- readFile filename
       (hPutStr stdout . show)
           (let sexps s' = case parse pSexps filename s' of
                             Left _ -> [Ssym "#<parse-error>"]
                             Right es -> es
            in map evalSexp (sexps s))
