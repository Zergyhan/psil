-- TP-1  --- Implantation d'une sorte de Lisp          -*- coding: utf-8 -*-
{-# OPTIONS_GHC -Wall #-}
--
-- Ce fichier défini les fonctionalités suivantes:
-- - Analyseur lexical
-- - Analyseur syntaxique
-- - Pretty printer
-- - Implantation du langage

---------------------------------------------------------------------------
-- Importations de librairies et définitions de fonctions auxiliaires    --
---------------------------------------------------------------------------

import Text.ParserCombinators.Parsec -- Bibliothèque d'analyse syntaxique.
import Data.Char                -- Conversion de Chars de/vers Int et autres.
import System.IO                -- Pour stdout, hPutStr

---------------------------------------------------------------------------
-- 1ère représentation interne des expressions de notre language         --
---------------------------------------------------------------------------
data Sexp = Snil                        -- La liste vide
          | Scons Sexp Sexp             -- Une paire
          | Ssym String                 -- Un symbole
          | Snum Int                    -- Un entier
          -- Génère automatiquement un pretty-printer et une fonction de
          -- comparaison structurelle.
          deriving (Show, Eq)

-- Exemples:
-- (+ 2 3)  ==  (((() . +) . 2) . 3)
--          ==>  Scons (Scons (Scons Snil (Ssym "+"))
--                            (Snum 2))
--                     (Snum 3)
--
-- (/ (* (- 68 32) 5) 9)
--     ==  (((() . /) . (((() . *) . (((() . -) . 68) . 32)) . 5)) . 9)
--     ==>
-- Scons (Scons (Scons Snil (Ssym "/"))
--              (Scons (Scons (Scons Snil (Ssym "*"))
--                            (Scons (Scons (Scons Snil (Ssym "-"))
--                                          (Snum 68))
--                                   (Snum 32)))
--                     (Snum 5)))
--       (Snum 9)

---------------------------------------------------------------------------
-- Analyseur lexical                                                     --
---------------------------------------------------------------------------

pChar :: Char -> Parser ()
pChar c = do { _ <- char c; return () }

-- Les commentaires commencent par un point-virgule et se terminent
-- à la fin de la ligne.
pComment :: Parser ()
pComment = do { pChar ';'; _ <- many (satisfy (\c -> not (c == '\n')));
                (pChar '\n' <|> eof); return ()
              }
-- N'importe quelle combinaison d'espaces et de commentaires est considérée
-- comme du blanc.
pSpaces :: Parser ()
pSpaces = do { _ <- many (do { _ <- space ; return () } <|> pComment);
               return () }

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

-- La notation "'E" est équivalente à "(shorthand-quote E)"
-- La notation "`E" est équivalente à "(shorthand-backquote E)"
-- La notation ",E" est équivalente à "(shorthand-comma E)"
pQuote :: Parser Sexp
pQuote = do { c <- satisfy (\c -> c `elem` "'`,"); pSpaces; e <- pSexp;
              return (Scons
                      (Scons Snil
                             (Ssym (case c of
                                     ',' -> "shorthand-comma"
                                     '`' -> "shorthand-backquote"
                                     _   -> "shorthand-quote")))
                      e) }

-- Une liste (Tsil) est de la forme ( [e .] {e} )
pTsil :: Parser Sexp
pTsil = do _ <- char '('
           pSpaces
           (do { _ <- char ')'; return Snil }
            <|> do hd <- (do e <- pSexp
                             pSpaces
                             (do _ <- char '.'
                                 pSpaces
                                 return e
                              <|> return (Scons Snil e)))
                   pLiat hd)
    where pLiat :: Sexp -> Parser Sexp
          pLiat hd = do _ <- char ')'
                        return hd
                 <|> do e <- pSexp
                        pSpaces
                        pLiat (Scons hd e)

-- Accepte n'importe quel caractère: utilisé en cas d'erreur.
pAny :: Parser (Maybe Char)
pAny = do { c <- anyChar ; return (Just c) } <|> return Nothing

-- Une Sexp peut-être une liste, un symbol ou un entier.
pSexpTop :: Parser Sexp
pSexpTop = do { pTsil <|> pQuote <|> pSymbol
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
    readsPrec _ s = case parse pSexp "" s of
                      Left _ -> []
                      Right e -> [(e,"")]

---------------------------------------------------------------------------
-- Sexp Pretty Printer                                                   --
---------------------------------------------------------------------------

showSexp' :: Sexp -> ShowS
showSexp' Snil = showString "()"
showSexp' (Snum n) = showsPrec 0 n
showSexp' (Ssym s) = showString s
showSexp' (Scons e1 e2) = showHead (Scons e1 e2) . showString ")"
    where showHead (Scons Snil e') = showString "(" . showSexp' e'
          showHead (Scons e1' e2')
            = showHead e1' . showString " " . showSexp' e2'
          showHead e = showString "(" . showSexp' e . showString " ."

-- On peut utiliser notre pretty-printer pour la fonction générique "show"
-- (utilisée par la boucle interactive de GHCi).  Mais avant de faire cela,
-- il faut enlever le "deriving Show" dans la déclaration de Sexp.
{-
instance Show Sexp where
    showsPrec p = showSexp'
-}

-- Pour lire et imprimer des Sexp plus facilement dans la boucle interactive
-- de Hugs/GHCi:
readSexp :: String -> Sexp
readSexp = read
showSexp :: Sexp -> String
showSexp e = showSexp' e ""

---------------------------------------------------------------------------
-- Représentation intermédiaire                                          --
---------------------------------------------------------------------------

type Var = String

data Ltype = Lint
           | Llist Ltype
           | Lfun Ltype Ltype
           deriving (Show, Eq)

data Lexp = Lnum Int            -- Constante entière.
          | Lvar Var            -- Référence à une variable.
          | Lannot Lexp Ltype   -- Annotation de type.
          | Linvoke Lexp Lexp   -- Appel de fonction, avec un argument.
          | Lnil Ltype          -- Constructeur de liste vide.
          | Lcons Lexp Lexp     -- Constructeur de liste.
          | Lcase Lexp Lexp Var Var Lexp -- Expression conditionelle.
          | Llet Var Lexp Lexp  -- Déclaration de variable locale.
          -- Déclaration de fonction locale.
          | Lletrec Var [(Var, Ltype)] Ltype Lexp Lexp
          deriving (Show, Eq)

-- Première passe simple qui analyse un Sexp et construit une Lexp équivalente.
s2l :: Sexp -> Lexp
s2l (Snum n) = Lnum n
s2l (Ssym s) = Lvar s
s2l (Scons (Scons Snil (Ssym "nil")) se) = Lnil (s2t se)
-- Cons lists
s2l (Scons (Scons (Scons Snil (Ssym "cons")) e1) initalList) = (Lcons (s2l e1) (s2l initalList))
-- Let expressions
s2l (Scons (Scons (Scons Snil (Ssym "let")) (Scons Snil (Scons (Scons Snil (Ssym var)) val))) body) = (Llet var (s2l val) (s2l body))
--  (Scons (Scons (Scons Snil (Ssym "let")) (Scons Snil (Scons (Scons Snil (Ssym "x")) (Snum 5)))) (Scons (Scons (Scons Snil (Ssym "*")) (Ssym "x")) (Snum 4))

-- Appel de fonction
s2l (Scons (Scons Snil (Ssym op)) re) = (Linvoke (s2l (Ssym op)) (s2l re))
--s2l (Scons (Scons (Scons Snil (Ssym op)) Snum 5)
-- ((+ 2) 3) => Lexp? (Lnum 5 : Lint) (Lnum 5)
-- (Linvoke (Linvoke (Lvar "+") (Lnum 3)) (Lnum 2))
-- tenv0 "+" = Lfun Lint (Lfun Lint Lint)
s2l (Scons (Scons Snil (Scons (Scons Snil (Ssym op)) e1)) e2) =  (Linvoke (Linvoke (s2l (Ssym op)) (s2l e1)) (s2l e2))
--s2l (Scons (Scons Snil (Ssym op)) re) =  (Linvoke (s2l (Ssym op)) (s2l e1) (s2l re))
-- TODO: FAIRE CAS e1 e2 e3 .. en

--TODO: FAIRE CAS (+ 2 5)
s2l (Scons (Scons (Scons Snil (Ssym op)) e1) e2) = (Linvoke (Linvoke (s2l (Ssym op)) (s2l e1)) (s2l e2))
-- TODO: FAIRE CAS e1 e2 e3 .. en

--((+ 2) 5)
-- =
-- Scons (Scons Snil
--       (Scons (Scons Snil (Ssym "+")) (Snum 2))) (Snum 5)
--(+ 2 5)
-- =
-- Scons (Scons (Scons Snil (Ssym "+")) (Snum 2)) (Snum 5)


-- Annotation de type
s2l (Scons (Scons (Scons Snil (Ssym ":")) e) t) = (s2l e)
--Lannot Lexp Ltype   -- Annotation de type.
-- (: (+ 5) (Int -> Int))                  ; <function> : Int -> Int
-- equiv
-- Scons (Scons (Scons Snil (Ssym ":")) (Scons (Scons Snil (Ssym "+")) (Snum 5))) (Scons (Scons (Scons Snil (Ssym "Int")) (Ssym "->")) (Ssym "Int"))



-- Construction de listes avec cons (sans sucre syntaxique)
-- MAYBE? cas (cons [] [])

-- cas (cons e1 [])

-- cas (cons [] e2)

-- cas (cons e1 e2)

--(cons 5 (nil Int))                      ; [5] : List Int
--data Ltype = | Llist Ltype
--data Lexp = | Lcons Lexp Lexp     -- Constructeur de liste.

--(cons 1 2)
--Scons (Scons (Scons Snil (Ssym "cons")) (Snum 1)) (Snum 2)
--
--(cons 5 (nil Int))
--Scons (Scons (Scons Snil (Ssym "cons")) (Snum 5)) (Scons (Scons Snil (Ssym "nil")) (Ssym "Int"))
--
--(cons (nil Int) 5)
--Scons (Scons (Scons Snil (Ssym "cons")) (Scons (Scons Snil (Ssym "nil")) (Ssym "Int"))) (Snum 5)
--
--
--(cons 5 [blabla])
--Scons (Scons (Scons Snil (Ssym "cons")) (Snum 5)) (s2l blabla)

--slip-all_2008_1.hs:
--Lexp =  Lfix [(Var,Lexp)] Lexp
-- Let:
--compile (Scons (Ssym "let")
--               (Scons bindings (Scons body Snil))) =
--    Lfix (parseFixBinds bindings) (compile body)          --Lfix ici
--
--compile (sexp@(Scons (Ssym "let") _)) =
--    error ("unknown let exp: " ++ show sexp)

--tp1_bitbucket.hs:
--Lexp =  Lfix [(Var,Lexp)] Lexp
--bof...


s2l se = error ("Malformed Psil: " ++ (showSexp se))


--       whatever
--       / \
--   s2lInv s2lre

--s2l
--(_
--  (Scons
--    (Scons
--      (Scons Snil (Ssym op))
--    e1)
--  re@_))
-- (Linvoke (s2l (Ssym op)) (s2l e1))
----s2l (Scons op args@(Scons _ _)) =
----    sfoldl (\fonction -> \argument -> Linvoke fonction (s2l argument)) (s2l op) args
--s2l (Scons op (Scons e1 Snil))
--s2l (_ (Scons (Snil op) e1)
---- Scons (Scons Snil e1) e2 => ((e1) e2) e1 => ((e1) e2)
--
--Scons (Scons Snil (Ssym "+")) (Snum 2)
--
--
--(Scons
--(Scons (Scons Snil (Ssym "+")) (Snum 2))
--(Snum 3))
--
--(Scons
--(Scons (Scons (Scons Snil (Ssym "+")) (Snum 2)) (Snum 3))
--(Snum 4))
--
--
--
--Scons (Scons Snil (Scons (Scons Snil (Ssym "+")) (Snum 2))) (Snum 3) -- ((+ 2) 3)
--s2l (Scons (Scons Snil (Ssym op)) re@(Scons _ _)) = (\f -> \a -> Linvoke f (s2l a)) (s2l op) re
--s2l (Scons (Scons Snil (Ssym op)) re@(Scons _ _)) = (Linvoke (s2l (Ssym op)) (s2l re))


-- foldl sur des s-listes
--sfoldl f base Snil            = base
--sfoldl f base (Scons car cdr) = sfoldl f (f base car) cdr
--
--s2l (Scons f (Scons arg Snil)) = Linvoke (s2l f) (s2l arg)
--s2l (Scons op args@(Scons _ _)) =
--    sfoldl (\fonction -> \argument -> Linvoke fonction (s2l argument)) (s2l op) args





----data Lexp = Linvoke Lexp [Lexp]
--compile (Scons (Scons Snil f) arg) = Lapp (compile f) [(compile arg)]
----by default we perform a function application
--compile (Scons e1 e2) = Lapp (compile e1) [(compile e2)]
-- Generic in built operator
-- (Scons (Scons (Scons Snil (Ssym "+")) (Snum 1)) (Snum 2))

--
--s2l (Scons
--
------------------other years------------------------
----solupartiellea21
--s2l (Scons firstArg fnExp) =
--    let buildPipe :: Lexp -> Sexp -> Lexp
--        buildPipe arg (Scons fn Snil) = Lpipe arg (s2l fn)
--        buildPipe arg (Scons fn rem) = buildPipe (Lpipe arg (s2l fn)) rem
--    in buildPipe (s2l firstArg) fnExp
--
----corrigé 2008
---- foldl sur des s-listes
--sfoldl f base Snil            = base
--sfoldl f base (Scons car cdr) = sfoldl f (f base car) cdr
----
--s2l (Scons op (Scons Snil re)) = Linvoke (s2l op) (compile re)
--s2l (Scons op res@(Scons _ _)) =
--    sfoldl (\fun -> \re -> Linvoke fun (s2l re)) (s2l op) res
--s2l (sexp@(Scons _ _)) =
--    error ("unknown function application: " ++ show sexp)
--
----chrisa21
---- Appel de fonction à un ou plusieurs arguments
--s2l (Scons (Scons (Ssym e0) (Scons es Snil)))
--    = Lpipe (Lpipe e0 (s2l es))
--
----bitbucket
----by default we perform a function application
--s2l (Scons e1 e2) = Lapp (s2l e1) (s2l e2)
--Lapp Lexp Lexp
------------------end of other years------------------------


--data Ltype = Lint
--           | Llist Ltype
--           | Lfun Ltype Ltype
--data Sexp = Snil                        -- La liste vide
--          | Scons Sexp Sexp             -- Une paire
--          | Ssym String                 -- Un symbole
--          | Snum Int                    -- Un entier


s2t :: Sexp -> Ltype
s2t (Ssym "Int") = Lint
-- Type d'une liste - t ::= (List t)
s2t (Scons (Scons (Snil) (Ssym "List")) typeListe) = (Llist (s2t typeListe))
-- Type d'une fonction t ::= (t1 ... tn -> t <=> t1 -> t2 - > ...(tn -> t)...))
s2t (Scons (Scons Snil t1) (typeReste)) = (Lfun (s2t t1) (s2t typeReste))
-- ¡¡¡ COMPLETER ICI !!! --
s2t se = error ("Malformed Psil type: " ++ (showSexp se))

---------------------------------------------------------------------------
-- Vérification des types                                                --
---------------------------------------------------------------------------

--data Ltype = Lint
--           | Llist Ltype
--           | Lfun Ltype Ltype
--data Lexp = Lnum Int            -- Constante entière.
--          | Lvar Var            -- Référence à une variable.
--          | Lannot Lexp Ltype   -- Annotation de type.
--          | Linvoke Lexp Lexp   -- Appel de fonction, avec un argument.
--          | Lnil Ltype          -- Constructeur de liste vide.
--          | Lcons Lexp Lexp     -- Constructeur de liste.
--          | Lcase Lexp Lexp Var Var Lexp -- Expression conditionelle.
--          | Llet Var Lexp Lexp  -- Déclaration de variable locale.\
type TEnv = Var -> Ltype

check :: TEnv -> Lexp -> Ltype
check _tenv (Lnum _) = Lint
check tenv (Lvar v) = tenv v
-- ¡¡¡ COMPLETER ICI !!! --


tenv0 :: TEnv
tenv0 "+" = Lfun Lint (Lfun Lint Lint)
tenv0 "-" = Lfun Lint (Lfun Lint Lint)
tenv0 "*" = Lfun Lint (Lfun Lint Lint)
tenv0 "/" = Lfun Lint (Lfun Lint Lint)
tenv0 x   = error ("Uknown variable: " ++ show x)

tlookup :: TEnv -> Var -> Ltype
tlookup env = env

tinsert :: TEnv -> Var -> Ltype -> TEnv
tinsert env var val = \x -> if (x == var) then val else tlookup env x

---------------------------------------------------------------------------
-- Évaluateur                                                            --
---------------------------------------------------------------------------

-- Type des valeurs renvoyées par l'évaluateur.
data Value = Vnum Int
           | Vnil
           | Vcons Value Value
           | Vlambda (Value -> Value)

instance Show Value where
    showsPrec p (Vnum n) = showsPrec p n
    showsPrec _p Vnil = showString "[]"
    showsPrec p (Vcons v1 v2) =
        let showTail Vnil = showChar ']'
            showTail (Vcons v1' v2') =
                showChar ' ' . showsPrec p v1' . showTail v2'
            showTail v = showString " . " . showsPrec p v . showChar ']'
        in showChar '[' . showsPrec p v1 . showTail v2
    showsPrec _p _ = showString "<function>"

type VEnv = Var -> Value

-- L'environnement initial qui contient les fonctions prédéfinies.
env0 :: VEnv
env0 "+"    = Vlambda (\ (Vnum x) -> Vlambda (\ (Vnum y) -> Vnum (x + y)))
env0 "*"    = Vlambda (\ (Vnum x) -> Vlambda (\ (Vnum y) -> Vnum (x * y)))
env0 "/"    = Vlambda (\ (Vnum x) -> Vlambda (\ (Vnum y) -> Vnum (x `div` y)))
env0 "-"    = Vlambda (\ (Vnum x) -> Vlambda (\ (Vnum y) -> Vnum (x - y)))
env0 x      = error ("Uknown variable: " ++ show x)

vlookup :: VEnv -> Var -> Value
vlookup env = env

vinsert :: VEnv -> Var -> Value -> VEnv
vinsert env var val = \x -> if (x == var) then val else vlookup env x


-- La fonction d'évaluation principale.
eval :: VEnv -> Lexp -> Value
eval _env (Lnum n) = Vnum n
eval env (Lvar x) = vlookup env x
eval env (Lannot e _) = eval env e
-- ¡¡¡ COMPLETER ICI !!! --
-- Example (+ 2)/((+ 2) 3) => Linvoke (Lvar "+") (Lnum 2) /
-- Linvoke (Linvoke (Lvar "+") (Lnum 2)) (Lnum 3)
-- eval env (Linvoke (f) (exp)) = (env f) (eval env exp)
--eval env (Linvoke (f) (exp)) =
--eval env (Linvoke e1 e2) = if ((eval env e1) == (Vlambda f)) then (f (eval env e2))
eval env (Linvoke e re) =
  case eval env e of
    Vlambda f -> f (eval env re)
    _ -> error ("Not a function: " ++ show e)

eval env (Llet x val body) =
    let v = eval env val
        env' y = if (x == y) then v else env y
    in eval env' body


--eval env (Linvoke e1 e2) =
--    case eval env e1 of
--      Vlambda f -> f (eval env e2)
--      _ -> error "Not a function"

--eval env (Linvoke e1 e2) =
--  case eval env e1 of
--    Vlambda f -> f (eval env e2)
--    _ -> error "Not a function"


-- Liste vide
eval env (Lnil _) = Vnil
-- Lnil Ltype          -- Constructeur de liste vide.
-- s2l (Scons (Scons Snil (Ssym "nil")) se) = Lnil (s2t se)



-- Construction de listes
eval _env (Lcons e1 e2) = Vcons (eval _env e1) (eval _env e2)
--s2l (Scons (Scons (Scons Snil (Ssym "cons")) e1) initalList) = (Lcons (s2l e1) (s2l initalList))
-- a Lexp = | Lcons Lexp Lexp     -- Constructeur de liste.
--data Value = | Vcons Value Value






--
--
--data Lexp = Lnum Int
--          | Lvar Var
--          | Llambda Var Lexp
--          | Lapp Lexp Lexp
--          -- Déclaration simple d'une variable non-récursive.
--          | Llet Var Lexp Lexp
--          -- Déclaration d'une liste de variables qui peuvent être
--          -- mutuellement récursives.
--          | Lfix [(Var,Lexp)] Lexp
--          deriving (Show, Eq)
---- Évaluateur                                                            --
-----------------------------------------------------------------------------
--
---- Type des valeurs renvoyées par l'évaluateur.
--data Value = Vnum Int
--           | Vnil
--           | Vcons Value Value
--           | Vfun (Value -> Value) --Vlampda
--
--
--instance Show Value where
--    showsPrec p (Vnum n) = showsPrec p n
--    showsPrec p Vnil = showString "[]"
--    showsPrec p (Vcons v1 v2) =
--        let showTail Vnil = showChar ']'
--            showTail (Vcons v1 v2) =
--                showChar ' ' . showsPrec p v1 . showTail v2
--            showTail v = showString " . " . showsPrec p v . showChar ']'
--        in showChar '[' . showsPrec p v1 . showTail v2
--    showsPrec p _ = showString "<function>"
--
--type Env = Var -> Value --Env c'est Venv
--
---- L'environnement initial qui contient les fonctions prédéfinies.
--env0 :: Env
--env0 "cons" = Vfun (\x -> Vfun (\y -> Vcons x y))
--env0 "nil"  = Vnil
--env0 "car"  = Vfun (\ (Vcons x y) -> x)
--env0 "cdr"  = Vfun (\ (Vcons x y) -> y)
--env0 "cons" = Vfun (\x -> Vfun (\y -> Vcons x y))
--env0 "ifnil"= Vfun (\x -> case x of Vnil      -> Vfun (\y -> Vfun (\_ -> y))
--                                    Vcons _ _ -> Vfun (\_ -> Vfun (\y -> y))
--                                    _ -> error "Not a list")
--env0 "+"    = Vfun (\ (Vnum x) -> Vfun (\ (Vnum y) -> Vnum (x + y)))
--env0 "*"    = Vfun (\ (Vnum x) -> Vfun (\ (Vnum y) -> Vnum (x * y)))
--env0 "/"    = Vfun (\ (Vnum x) -> Vfun (\ (Vnum y) -> Vnum (x `div` y)))
--env0 "-"    = Vfun (\ (Vnum x) -> Vfun (\ (Vnum y) -> Vnum (x - y)))
--env0 x      = error ("Uknown variable: " ++ show x)
--
--elookup :: Env -> Var -> Value
--elookup env = env
--
--einsert :: Env -> Var -> Value -> Env
--einsert env var val = \x -> if (x == var) then val else elookup env x
----vinsert :: VEnv -> Var -> Value -> VEnv
----vinsert env var val = \x -> if (x == var) then val else vlookup env x
--
---- La fonction d'évaluation principale.
--eval :: Env -> Lexp -> Value
--eval env (Lnum n) = Vnum n
--eval env (Lvar x) = elookup env x
--eval env (Llambda x e) =
--    Vfun (\v -> eval (einsert env x v) e)
--eval env (Lapp e1 e2) =
--    case eval env e1 of
--      Vfun f -> f (eval env e2)
--      _ -> error "Not a function"
--eval env (Llet x e1 e2) =
--    let v = eval env e1
--        newenv y = if (x == y) then v else env y
--    in eval newenv e2
--eval env (Lfix xs e) =
--    let lookup [] y = env y
--        lookup ((x,e):xs) y = if x == y then eval newenv e else lookup xs y
--        newenv = lookup xs
--    in eval newenv e



---------------------------------------------------------------------------
-- Toplevel                                                              --
---------------------------------------------------------------------------

-- Lit un fichier contenant plusieurs Sexps, les évalues l'une après
-- l'autre, et renvoie la liste des valeurs obtenues.
run :: FilePath -> IO ()
run filename
  = do filestring <- readFile filename
       (hPutStr stdout)
           (let sexps s = case parse pSexps filename s of
                            Left err -> error ("Parse error: " ++ show err)
                            Right es -> es
            in (concat
                (map (\ sexp -> let { lexp = s2l sexp
                                   ; ltyp = check tenv0 lexp
                                   ; val = eval env0 lexp }
                               in "  " ++ show val
                                  ++ " : " ++ show ltyp ++ "\n")
                     (sexps filestring))))

sexpOf :: String -> Sexp
sexpOf = read

lexpOf :: String -> Lexp
lexpOf = s2l . sexpOf

typeOf :: String -> Ltype
typeOf = check tenv0 . lexpOf

valOf :: String -> Value
valOf = eval env0 . lexpOf
