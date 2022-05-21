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

-- Construction de structure avec aucun élément
s2l (Scons (Ssym "cons" (Scons (Ssym tag) Snil))) 
    = Lcons tag []

-- Construction de structure avec un ou plusieurs éléments
s2l (Scons (Ssym "cons") (Scons (Ssym tag) (Scons contenu Snil))) 
    = Lcons tag [s2l contenu]

-- Appel de fonction à un ou plusieurs arguments
s2l (Scons (Scons (Ssym e0) (Scons es Snil))) 
    = Lpipe (Lpipe e0 (s2l es))

-- Filtrage avec une seule branche par défaut  : (case e (_ e1))
s2l (Scons (Ssym "case") (Scons e (Scons (Scons (Ssym "_")
    (Scons e1 Snil)) Snil)))
    = Lcase (s2l e) [(Nothing,e1)]

-- Filtrage avec une seule branche de filtrage : (case e ((tag x1 x2) e1))
s2l (Scons (Ssym "case") (Scons e (Scons (Scons (Scons (Ssym tag) 
    (Scons (Ssym x1) (Scons (Ssym x2) Snil))) (Scons (Ssym e1) Snil)) Snil)))
    = Lcase (s2l e) [((tag,[x1,x2]),e1)]

-- Filtrage sur les listes : (case e b1 ... bn)
s2l (Scons (Ssym "case") (Scons e (Scons (Ssym b1) (Scons bs Snil))))
    = Lcase (s2l e) [()]

-- Bloc if then else : (if e1 e2 e3) -> (case e1 ((true) e2) ((false) e3))
s2l (Scons (Ssym "if") (Scons (Scons e1 (Scons e2 (Scons e3 Snil)))))
    = s2l (Scons (Ssym "case") (Scons e1 (Scons (Scons (Ssym "true") 
        (Scons e2 Snil)) (Scons (Scons (Ssym "false") (Scons e3 Snil)) Snil))))

-- Élimination du sucre syntaxique pour la déclaration de fonction :
-- ((x x1 ... xn) e) -> (x (lambda (x1 ... xn) e))
s2l (Scons (Scons (Ssym x) (Scons (Ssym x1) (Scons xs Snil))) (Scons e1 Snil))
    = s2l ((Scons (Ssym x) (Scons (Scons (Ssym "lambda") 
        (Scons (Scons (Ssym x1) (Scons xs Snil))) (Scons e1 Snil))) Snil))

-- slet - déclaration locale statique (slet ((x x1 ... xn) e1) e)
-- slet avec une variable, où d est une déclaration de variable (x e1)
s2l (Scons (Ssym "slet") (Scons (Scons (Scons (Ssym x) 
    (Scons e1 Snil)) Snil) (Scons e Snil)))
    = Llet Lexical x e1 e

-- slet avec une variable, où d est une déclaration de fonction : 
-- ((x x1 ... xn) e1)
s2l (Scons (Ssym "slet") (Scons (Scons (Scons (Ssym x) (Scons (Ssym x1) 
    (Scons xs Snil)))) (Scons e1 Snil)) (Scons e Snil))
    = Llet Lexical x (Lfn x1 (s2l (Scons (Ssym "lambda") 
        (Scons xs (Scons e1 Snil))))) e

-- slet avec plusieurs variables et élimination du sucre syntaxique :
-- (slet (d1 ... dn) e) -> (slet (d1) (slet (... dn) e))
s2l (Scons (Ssym "slet") (Scons (Scons (Ssym d1) (Scons ds Snil)) 
    (Scons e Snil)))
    = Llet Lexical x d1 s2l(Scons (Ssym "slet") (Scons (Scons ds Snil) 
        (Scons e Snil)))

-- dlet - déclaration locale dynamique (dlet ((x x1 ... xn) e1) e)
-- dlet avec une variable, où d est une déclaration de variable (x e1)
s2l (Scons (Ssym "dlet") (Scons (Scons (Scons (Ssym x) (Scons e1 Snil)) Snil) 
    (Scons e Snil)))
    = Llet Dynamic x e1 e

-- dlet avec une variable, où d est une déclaration de fonction : 
-- ((x x1 ... xn) e1)
s2l (Scons (Ssym "dlet") (Scons (Scons (Scons (Ssym x) (Scons (Ssym x1) 
    (Scons xs Snil)))) (Scons e1 Snil)) (Scons e Snil))
    = Llet Dynamic x (Lfn x1 (s2l (Scons (Ssym "lambda") (Scons xs 
        (Scons e1 Snil))))) e

-- dlet avec plusieurs variables et élimination du sucre syntaxique :
-- (dlet (d1 ... dn) e) -> (dlet (d1) (slet (... dn) e))
s2l (Scons (Ssym "dlet") (Scons (Scons (Ssym d1) (Scons ds Snil)) 
    (Scons e Snil)))
    = Llet Dynamic x d1 s2l(Scons (Ssym "dlet") (Scons (Scons ds Snil) 
        (Scons e Snil)))
        
-- Fonction de n arguments
-- (lambda (x1 ... xn) e) -> (lambda (x1) (lambda (x2) ... lambda (xn) e)..)
-- cas de base avec une seule variable x1
s2l (Scons (Ssym "lambda") (Scons (Scons (Ssym x1) Snil) (Scons e Snil)))
    = Lfn x1 (s2l e)

-- cas avec deux ou plusieurs variables x1 ... xn (x2 ... xn = xs)
s2l (Scons (Ssym "lambda") (Scons (Scons (Ssym x1) (Scons xs Snil)) 
    (Scons e Snil)))
    = Lfn x1 (s2l (Scons (Ssym "lambda") (Scons xs (Scons e Snil))))


s2l se = error ("Malformed Sexp: " ++ (showSexp se))

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

-- Fonction auxiliaire qui cherche une variable dans l'environnement
-- et retourne sa valeur.
-- Inspirée de l'exercice 4.5.
elookup :: Var -> Env -> Value
elookup x ((x1, v1) : _) | x == x1 = v1
elookup x (_ : env) = elookup x env
elookup x [] = error ("la variable \"" <> x <> "\" n'existe pas")

-- Implémentation de la fonction (==) à l'aide du pattern matching
-- pour pouvoir comparer BindingType.
-- source: https://stackoverflow.com/a/56406723
instance Eq BindingType where
    Lexical == Lexical = True
    Dynamic == Dynamic = True
    _ == _ = False

eval :: Env -> Env -> Lexp -> Value
-- Évaluation d'un nombre
eval _senv _denv (Lnum n) = Vnum n

-- Évaluation d'une référence à une variable
eval senv _denv (Lvar x) = elookup x senv 

-- Évaluation d'une fonction anonyme
eval senv _denv (Lfn x e) = 
    let val = eval senv _denv x
    in Vfun (senv val)

-- Évaluation d'un appel de fonction
eval senv _denv (Lpipe exp arg) = 
    let (Vfn fn) = eval senv exp
        argValue = eval senv arg
    in fn argValue

-- Évaluation d'un constructeur de liste
eval senv _denv (Lcons tag eList) = 
    Vons tag (eval senv eList)

-- Évaluation d'une expression conditionnelle
eval senv _denv (Lcase v ((_, e):[])) = eval senv _denv e
eval senv _denv (Lcase v ((pat, e):bs))
    | v == snd pat = eval senv _denv e
    | otherwise = eval senv _denv (Lcase v bs)

    -- Évaluation d'une déclaration de variable locale
eval _senv _denv (Llet scope varName varExp mainExp)
    | scope == Lexical = eval ((varName, eval senv varExp) : senv) mainExp
    | scope == Dynamic = eval ((varName, eval denv varExp) : denv) mainExp
    | otherwise = error ("Portée non acceptée")

eval _ _ e = error ("Can't eval: " ++ show e)

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

            