module ProgViz.Adapt where

import           Control.Applicative      ((<$>))

import qualified Language.Python.Common   as Py
import qualified Language.Python.Version3 as Py

import qualified ProgViz.Types            as PV

testParse :: String -> PV.Statement
testParse str = case Py.parseModule (str ++ "\n") "test" of
  Left err       -> error $ "Parse failed!" ++ show err
  Right (res, _) -> convertModule res

convertModule :: Py.Module a -> PV.Statement
convertModule (Py.Module statements) = PV.combine $ map convertStatement statements

convertStatement :: Py.Statement a -> PV.Statement
convertStatement (Py.Assign { Py.assign_to = [lhs], Py.assign_expr = rhs }) =
  PV.Assign (convertExpr lhs) (convertExpr rhs)
convertStatement (Py.StmtExpr expr _) = PV.Expr $ convertExpr expr
convertStatement (Py.For [Py.Var var _] ls body _ _) =
  PV.For (Py.ident_string var) (convertExpr ls) (PV.combine $ convertStatement <$> body)
convertStatement _ = error "Unsupported language construct."

convertExpr :: Py.Expr a -> PV.Expr
convertExpr (Py.Var { Py.var_ident = Py.Ident { Py.ident_string = name } }) =
  PV.Var name
convertExpr (Py.Int { Py.int_value = n })                 = PV.NumLit n
convertExpr (Py.Bool { Py.bool_value = b })               = PV.BoolLit b
convertExpr (Py.Strings { Py.strings_strings = strings }) = PV.StrLit $ concat strings
convertExpr (Py.List { Py.list_exprs = exprs }) =
  PV.ListLit $ convertExpr <$> exprs
convertExpr (Py.BinaryOp { Py.operator = op, Py.left_op_arg = e₁, Py.right_op_arg = e₂ }) =
  PV.Bin (convertOp op) (convertExpr e₁) (convertExpr e₂)
convertExpr (Py.UnaryOp { Py.operator = op, Py.op_arg = arg }) =
  case op of
    Py.Not {}   -> PV.Un PV.Not (convertExpr arg)
    Py.Minus {} -> PV.Un PV.Neg (convertExpr arg)
    _           -> error "Invalid unary operator!"
convertExpr (Py.Call { Py.call_fun = fn, Py.call_args = args }) =
  case fn of
    Py.BinaryOp { Py.operator = Py.Dot {}, Py.left_op_arg = e₁, Py.right_op_arg = e₂ } ->
      case (e₁, e₂) of
        (Py.Var { Py.var_ident = name₁ }, Py.Var { Py.var_ident = name₂ }) ->
          PV.Call (Py.ident_string name₁)
                  (PV.toMethod (Py.ident_string name₂))
                  (convertExpr . extractExpr <$> args)
        _ -> error "Invalid method call!"
    _ -> error "Invalid method call!"
  where extractExpr (Py.ArgExpr { Py.arg_expr = expr }) = expr
        extractExpr _                                   = error "Unsupported syntax!"
convertExpr _ = error "Unsupported syntax!"

convertOp :: Py.Op a -> PV.Op
convertOp Py.LessThan {}          = PV.Lt
convertOp Py.GreaterThan {}       = PV.Gt
convertOp Py.LessThanEquals {}    = PV.Leq
convertOp Py.GreaterThanEquals {} = PV.Geq
convertOp Py.Equality {}          = PV.Eq
convertOp Py.NotEquals {}         = PV.Neq
convertOp Py.Multiply {}          = PV.Mult
convertOp Py.Plus {}              = PV.Plus
convertOp Py.Minus {}             = PV.Minus
convertOp Py.And {}               = PV.And
convertOp Py.Or {}                = PV.Or
convertOp _                       = error "Unsupported operator."
