module ProgViz.Adapt where

import           Control.Applicative    ((<$>))

import qualified Language.Python.Common as Py

import qualified ProgViz.Types          as PV

convertModule :: Py.Module a -> [PV.Statement]
convertModule (Py.Module statements) = map convertStatement statements

convertStatement :: Py.Statement a -> PV.Statement
convertStatement (Py.Assign { Py.assign_to = [lhs], Py.assign_expr = rhs }) =
  PV.Assign (convertExpr lhs) (convertExpr rhs)
convertStatement (Py.StmtExpr { Py.stmt_expr = expr}) =
  PV.Expr $ convertExpr expr
convertStatement _ = error "Unsupported language construct."

convertExpr :: Py.Expr a -> PV.Expr
convertExpr (Py.Var { Py.var_ident = Py.Ident { Py.ident_string = name } }) =
  PV.Var name
convertExpr (Py.Int { Py.int_value = n })                 = PV.NumLit n
convertExpr (Py.Bool { Py.bool_value = b })               = PV.BoolLit b
convertExpr (Py.Strings { Py.strings_strings = strings }) = PV.StrLit $ concat strings
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
