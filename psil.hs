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
-- Annotation de type
s2l (Scons (Scons (Scons Snil (Ssym ":")) (expression)) t) =
  (Lannot (s2l expression) (s2t t))
-- Constructeur de listes vides
s2l (Scons (Scons Snil (Ssym "nil")) se) = Lnil (s2t se)
-- Cons lists
s2l (Scons (Scons (Scons Snil (Ssym "cons")) e1) initalList) =
  (Lcons (s2l e1) (s2l initalList))
-- List list


-- Let expressions
s2l (Scons (Scons (Scons Snil (Ssym "let"))
  (Scons Snil (Scons (Scons Snil (Ssym var)) val))) body) =
    (Llet var (s2l val) (s2l body))
-- Fancy Let expressions


-- Appel de fonction
-- Sans sucre syntaxique
s2l (Scons (Scons Snil (Ssym op)) re) = (Linvoke (s2l (Ssym op)) (s2l re))
s2l (Scons (Scons Snil (Scons (Scons Snil (Ssym op)) e0)) e1) =
  (Linvoke (Linvoke (s2l (Ssym op)) (s2l e0)) (s2l e1))
s2l (Scons (Scons Snil (Scons (Scons Snil (
  Scons (Scons Snil (Ssym op)) e0)) e1)) e2) =
  (Linvoke (Linvoke (Linvoke (s2l (Ssym op)) (s2l e0)) (s2l e1)) (s2l e2))
s2l (Scons (Scons Snil (Scons (Scons Snil (
  Scons (Scons Snil (Scons (Scons Snil (Ssym op)) e0)) e1)) e2)) e3) =
  (Linvoke (Linvoke (Linvoke (Linvoke (s2l (Ssym op)) (s2l e0)) (s2l e1))
  (s2l e2)) (s2l e3))
s2l (Scons (Scons Snil (Scons (Scons Snil
  (Scons (Scons Snil (Scons (Scons Snil (Scons (Scons Snil (Ssym op))
   e0)) e1)) e2)) e3)) e4) =
  (Linvoke (Linvoke (Linvoke (Linvoke (Linvoke (s2l (Ssym op))
  (s2l e0)) (s2l e1)) (s2l e2)) (s2l e3)) (s2l e4))
-- Avec sucre syntaxique
s2l (Scons (Scons (Scons Snil (Ssym op)) e0) e1) =
  (Linvoke (Linvoke (s2l (Ssym op)) (s2l e0)) (s2l e1))
s2l (Scons (Scons (Scons (Scons Snil (Ssym op)) e0) e1) e2) =
  (Linvoke (Linvoke (Linvoke (s2l (Ssym op)) (s2l e0)) (s2l e1)) (s2l e2))
s2l (Scons (Scons (Scons (Scons (Scons Snil (Ssym op)) e0) e1) e2) e3) =
  (Linvoke (Linvoke (Linvoke (Linvoke (s2l (Ssym op)) (s2l e0)) (s2l e1))
  (s2l e2)) (s2l e3))
s2l (Scons (Scons (Scons (Scons (Scons (Scons Snil (Ssym op)) e0) e1) e2) e3)
 e4) =
  (Linvoke (Linvoke (Linvoke (Linvoke (Linvoke (s2l (Ssym op)) (s2l e0))
  (s2l e1)) (s2l e2)) (s2l e3)) (s2l e4))


s2l se = error ("Malformed Psil: " ++ (showSexp se))


s2t :: Sexp -> Ltype
s2t (Ssym "Int") = Lint

-- Type d'une liste - t ::= (List t)
s2t (Scons (Scons (Snil) (Ssym "List")) typeListe) = (Llist (s2t typeListe))

-- Type d'une fonction t ::= (t1 ... tn -> t <=> t1 -> t2 - > ...(tn -> t)...))
s2t (Scons (Scons Snil t1) (typeReste)) = (Lfun (s2t t1) (s2t typeReste))
s2t se = error ("Malformed Psil type: " ++ (showSexp se))

---------------------------------------------------------------------------
-- Vérification des types                                                --
---------------------------------------------------------------------------
type TEnv = Var -> Ltype

check :: TEnv -> Lexp -> Ltype
check _tenv (Lnum _) = Lint
check tenv (Lvar v) = tenv v
check tenv (Lannot e _) = check tenv e
-- Vérification de l'appel de fonctions
check tenv (Linvoke e1 e2) = prune (check tenv e1) (check tenv e2)
check tenv (Lnil t) = (Llist t)
check tenv (Lcons e1 e2) = fix (check tenv e2)
check tenv (Llet var val body) = check (
  extend tenv var (check tenv val)) body

-- Retourne le type t2, le type du retour de fonction, si le
-- premier élément est une fonction qui prend en argument
-- une valeur du même type que le type de l'argument.
prune :: Ltype -> Ltype -> Ltype
prune (Lfun Lint (t2)) Lint = t2
prune (Lfun (Llist t) (e2)) (Llist t') = if t == t'
  then e2 else error "Type mismatch"

fix :: Ltype -> Ltype
fix (Lint) = (Llist Lint)
fix a = a

extend :: TEnv -> Var -> Ltype -> TEnv
extend tenv var t = \v -> if v == var then t else tenv v

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

-- Functions
eval env (Linvoke e re) =
  case eval env e of
    Vlambda f -> f (eval env re)
    _ -> error ("Not a function: " ++ show e)

-- Adding variables to the environment
eval env (Llet x val body) =
    let v = eval env val
        env' y = if (x == y) then v else env y
    in eval env' body

-- Liste vide
eval env (Lnil _) = Vnil

-- Construction de listes
eval _env (Lcons e1 e2) = Vcons (eval _env e1) (eval _env e2)


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
