/*
 * Use of this source code is governed by the MIT license that can be
 * found in the LICENSE file.
 */

package org.rust.lang.core.psi.ext

import com.intellij.lang.ASTNode
import com.intellij.psi.PsiElement
import com.intellij.psi.stubs.IStubElementType
import org.rust.lang.core.macros.RsExpandedElement
import org.rust.lang.core.psi.*
import org.rust.lang.core.resolve.KnownItems
import org.rust.lang.core.stubs.RsPlaceholderStub
import org.rust.lang.core.types.ty.TyInteger
import org.rust.lang.core.types.ty.TyPrimitive

/**
 * Extracts [RsLitExpr] raw value
 */
val RsLitExpr.stringLiteralValue: String? get() = (kind as? RsTextLiteral)?.value

enum class UnaryOperator {
    REF, // `&a`
    REF_MUT, // `&mut a`
    DEREF, // `*a`
    MINUS, // `-a`
    NOT, // `!a`
    BOX, // `box a`
}

val RsUnaryExpr.operatorType: UnaryOperator get() = when {
    mut != null -> UnaryOperator.REF_MUT
    and != null -> UnaryOperator.REF
    mul != null -> UnaryOperator.DEREF
    minus != null -> UnaryOperator.MINUS
    excl != null -> UnaryOperator.NOT
    box != null -> UnaryOperator.BOX
    else -> error("Unknown unary operator type: `$text`")
}

interface OverloadableBinaryOperator {
    val traitName: String
    val itemName: String
    val fnName: String
    val sign: String

    operator fun component1(): String = traitName
    operator fun component2(): String = itemName
    operator fun component3(): String = fnName
    operator fun component4(): String = sign

    fun findTrait(items: KnownItems): RsTraitItem? =
        items.findLangItem(itemName)
}

sealed class BinaryOperator

sealed class ArithmeticOp(
    override val traitName: String,
    override val itemName: String,
    override val sign: String
) : BinaryOperator(), OverloadableBinaryOperator {
    object ADD : ArithmeticOp("Add", "add", "+") // `a + b`
    object SUB : ArithmeticOp("Sub", "sub", "-") // `a - b`
    object MUL : ArithmeticOp("Mul", "mul", "*") // `a * b`
    object DIV : ArithmeticOp("Div", "div", "/") // `a / b`
    object REM : ArithmeticOp("Rem", "rem", "%") // `a % b`
    object BIT_AND : ArithmeticOp("BitAnd", "bitand", "&") // `a & b`
    object BIT_OR : ArithmeticOp("BitOr", "bitor", "|") // `a | b`
    object BIT_XOR : ArithmeticOp("BitXor", "bitxor", "^") // `a ^ b`
    object SHL : ArithmeticOp("Shl", "shl", "<<") // `a << b`
    object SHR : ArithmeticOp("Shr", "shr", ">>") // `a >> b

    override val fnName: String get() = itemName

    companion object {
        fun values(): List<ArithmeticOp> = listOf(ADD, SUB, MUL, DIV, REM, BIT_AND, BIT_OR, BIT_XOR, SHL, SHR)
    }
}

sealed class BoolOp : BinaryOperator()

sealed class LogicOp : BoolOp() {
    object AND : LogicOp() // `a && b`
    object OR : LogicOp() // `a || b`
}

sealed class EqualityOp(
    override val sign: String
) : BoolOp(), OverloadableBinaryOperator {
    object EQ : EqualityOp("==") // `a == b`
    object EXCLEQ : EqualityOp("!=") // `a != b`

    override val traitName: String = "PartialEq"
    override val itemName: String = "eq"
    override val fnName: String = "eq"

    override fun findTrait(items: KnownItems): RsTraitItem? = items.PartialEq

    companion object {
        fun values(): List<EqualityOp> = listOf(EQ, EXCLEQ)
    }
}

sealed class ComparisonOp(
    override val sign: String
) : BoolOp(), OverloadableBinaryOperator {
    object LT : ComparisonOp("<") // `a < b`
    object LTEQ : ComparisonOp("<=") // `a <= b`
    object GT : ComparisonOp(">") // `a > b`
    object GTEQ : ComparisonOp(">=") // `a >= b`

    override val traitName: String = "PartialOrd"
    override val itemName: String = "ord"
    override val fnName: String = "partial_cmp"

    override fun findTrait(items: KnownItems): RsTraitItem? = items.PartialOrd

    companion object {
        fun values(): List<ComparisonOp> = listOf(LT, LTEQ, GT, GTEQ)
    }
}

sealed class AssignmentOp : BinaryOperator() {
    object EQ : AssignmentOp() // `a = b`
}

sealed class ArithmeticAssignmentOp(
    override val traitName: String,
    override val itemName: String,
    override val sign: String
) : AssignmentOp(), OverloadableBinaryOperator {
    object ANDEQ : ArithmeticAssignmentOp("BitAndAssign", "bitand_assign", "&=") // `a &= b`
    object OREQ : ArithmeticAssignmentOp("BitOrAssign", "bitor_assign", "|=") // `a |= b`
    object PLUSEQ : ArithmeticAssignmentOp("AddAssign", "add_assign", "+=") // `a += b`
    object MINUSEQ : ArithmeticAssignmentOp("SubAssign", "sub_assign", "-=") // `a -= b`
    object MULEQ : ArithmeticAssignmentOp("MulAssign", "mul_assign", "*=") // `a *= b`
    object DIVEQ : ArithmeticAssignmentOp("DivAssign", "div_assign", "/=") // `a /= b`
    object REMEQ : ArithmeticAssignmentOp("RemAssign", "rem_assign", "%=") // `a %= b`
    object XOREQ : ArithmeticAssignmentOp("BitXorAssign", "bitxor_assign", "^=") // `a ^= b`
    object GTGTEQ : ArithmeticAssignmentOp("ShrAssign", "shr_assign", ">>=") // `a >>= b`
    object LTLTEQ : ArithmeticAssignmentOp("ShlAssign", "shl_assign", "<<=") // `a <<= b`

    override val fnName: String get() = itemName

    companion object {
        fun values(): List<ArithmeticAssignmentOp> = listOf(ANDEQ, OREQ, PLUSEQ, MINUSEQ, MULEQ, DIVEQ, REMEQ, XOREQ, GTGTEQ, LTLTEQ)
    }
}

private val ArithmeticAssignmentOp.nonAssignEquivalent: BinaryOperator get() = when (this) {
    ArithmeticAssignmentOp.ANDEQ -> LogicOp.AND
    ArithmeticAssignmentOp.OREQ -> LogicOp.OR
    ArithmeticAssignmentOp.PLUSEQ -> ArithmeticOp.ADD
    ArithmeticAssignmentOp.MINUSEQ -> ArithmeticOp.SUB
    ArithmeticAssignmentOp.MULEQ -> ArithmeticOp.MUL
    ArithmeticAssignmentOp.DIVEQ -> ArithmeticOp.DIV
    ArithmeticAssignmentOp.REMEQ -> ArithmeticOp.REM
    ArithmeticAssignmentOp.XOREQ -> ArithmeticOp.BIT_XOR
    ArithmeticAssignmentOp.GTGTEQ -> ArithmeticOp.SHR
    ArithmeticAssignmentOp.LTLTEQ -> ArithmeticOp.SHL
}

/**
 * Binary operator categories. These categories summarize the behavior
 * with respect to the builtin operations supported.
 * Inspired by rustc
 */
enum class BinOpCategory {
    /** &&, || -- cannot be overridden */
    Shortcircuit,

    /** <<, >> -- when shifting a single integer, rhs can be any integer type. For simd, types must match */
    Shift,

    /** +, -, etc -- takes equal types, produces same type as input, applicable to ints/floats/simd */
    Math,

    /** &, |, ^ -- takes equal types, produces same type as input, applicable to ints/floats/simd/bool */
    Bitwise,

    /** ==, !=, etc -- takes equal types, produces bools, except for simd, which produce the input type */
    Comparison
}

val BinaryOperator.category: BinOpCategory get() = when(this) {
    is ArithmeticOp -> when (this) {
        ArithmeticOp.SHL, ArithmeticOp.SHR -> BinOpCategory.Shift

        ArithmeticOp.ADD, ArithmeticOp.SUB, ArithmeticOp.MUL,
        ArithmeticOp.DIV, ArithmeticOp.REM -> BinOpCategory.Math

        ArithmeticOp.BIT_AND, ArithmeticOp.BIT_OR, ArithmeticOp.BIT_XOR -> BinOpCategory.Bitwise
    }
    LogicOp.AND, LogicOp.OR -> BinOpCategory.Shortcircuit

    EqualityOp.EQ, EqualityOp.EXCLEQ, ComparisonOp.LT,
    ComparisonOp.LTEQ, ComparisonOp.GT, ComparisonOp.GTEQ -> BinOpCategory.Comparison

    is ArithmeticAssignmentOp -> nonAssignEquivalent.category
    AssignmentOp.EQ -> error("Cannot take a category for assignment op")
}

val RsBinaryOp.operatorType: BinaryOperator get() = when (op) {
    "+" -> ArithmeticOp.ADD
    "-" -> ArithmeticOp.SUB
    "*" -> ArithmeticOp.MUL
    "/" -> ArithmeticOp.DIV
    "%" -> ArithmeticOp.REM
    "&" -> ArithmeticOp.BIT_AND
    "|" -> ArithmeticOp.BIT_OR
    "^" -> ArithmeticOp.BIT_XOR
    "<<" -> ArithmeticOp.SHL
    ">>" -> ArithmeticOp.SHR

    "&&" -> LogicOp.AND
    "||" -> LogicOp.OR

    "==" -> EqualityOp.EQ
    "!=" -> EqualityOp.EXCLEQ

    ">" -> ComparisonOp.GT
    "<" -> ComparisonOp.LT
    "<=" -> ComparisonOp.LTEQ
    ">=" -> ComparisonOp.GTEQ

    "=" -> AssignmentOp.EQ
    "&=" -> ArithmeticAssignmentOp.ANDEQ
    "|=" -> ArithmeticAssignmentOp.OREQ
    "+=" -> ArithmeticAssignmentOp.PLUSEQ
    "-=" -> ArithmeticAssignmentOp.MINUSEQ
    "*=" -> ArithmeticAssignmentOp.MULEQ
    "/=" -> ArithmeticAssignmentOp.DIVEQ
    "%=" -> ArithmeticAssignmentOp.REMEQ
    "^=" -> ArithmeticAssignmentOp.XOREQ
    ">>=" -> ArithmeticAssignmentOp.GTGTEQ
    "<<=" -> ArithmeticAssignmentOp.LTLTEQ

    else -> error("Unknown binary operator type: `$text`")
}

val RsBinaryExpr.operator: PsiElement get() = binaryOp.operator
val RsBinaryExpr.operatorType: BinaryOperator get() = binaryOp.operatorType

abstract class RsExprMixin : RsStubbedElementImpl<RsPlaceholderStub>, RsExpr {
    constructor(node: ASTNode) : super(node)
    constructor(stub: RsPlaceholderStub, nodeType: IStubElementType<*, *>) : super(stub, nodeType)

    override fun getContext(): PsiElement? = RsExpandedElement.getContextImpl(this)
}

tailrec fun unwrapParenExprs(expr: RsExpr): RsExpr =
    if (expr is RsParenExpr) unwrapParenExprs(expr.expr) else expr

val RsExpr.isAssignBinaryExpr: Boolean
    get() = this is RsBinaryExpr && this.operatorType is AssignmentOp

private const val MAX_EXPR_DEPTH: Int = 64

private val defaultExprPathResolver: (RsPathExpr) -> RsElement? = { it.path.reference.resolve() }

fun RsExpr.calculate(
    expectedTy: TyInteger,
    pathExprResolver: ((RsPathExpr) -> RsElement?) = defaultExprPathResolver
): Long? {

    fun eval(expr: RsExpr?, depth: Int): Long? {
        // To prevent SO we restrict max depth of expression
        if (depth >= MAX_EXPR_DEPTH) return null
        return when (expr) {
            is RsLitExpr -> expr.integerLiteralValue
                ?.removeSuffix(expectedTy.name)
                ?.toLongOrNull()
                ?.validValueOrNull(expectedTy)
            is RsPathExpr -> {
                val const = pathExprResolver(expr) as? RsConstant ?: return null
                if (!const.isConst) return null
                val path = (const.typeReference?.typeElement as? RsBaseType)?.path ?: return null
                val integerType = TyPrimitive.fromPath(path) as? TyInteger ?: return null
                if (integerType == expectedTy) eval(const.expr, depth + 1) else null
            }
            is RsParenExpr -> eval(expr.expr, depth + 1)
            is RsUnaryExpr -> {
                if (expr.operatorType != UnaryOperator.MINUS) return null
                val value = eval(expr.expr, depth + 1) ?: return null
                (-value).validValueOrNull(expectedTy)
            }
            is RsBinaryExpr -> {
                val op = expr.operatorType as? ArithmeticOp ?: return null
                val leftValue = eval(expr.left, depth + 1) ?: return null
                val rightValue = eval(expr.right, depth + 1) ?: return null
                // TODO: check overflow
                val result = when (op) {
                    ArithmeticOp.ADD -> leftValue + rightValue
                    ArithmeticOp.SUB -> leftValue - rightValue
                    ArithmeticOp.MUL -> leftValue * rightValue
                    ArithmeticOp.DIV -> if (rightValue == 0L) null else leftValue / rightValue
                    ArithmeticOp.REM -> if (rightValue == 0L) null else leftValue % rightValue
                    ArithmeticOp.BIT_AND -> leftValue and rightValue
                    ArithmeticOp.BIT_OR -> leftValue or rightValue
                    ArithmeticOp.BIT_XOR -> leftValue xor rightValue
                    // We can't simply convert `rightValue` to Int
                    // because after conversion of quite large Long values (> 2^31 - 1)
                    // we can get any Int value including negative one
                    // so it can lead to incorrect result.
                    // But if `rightValue` >= `java.lang.Long.BYTES`
                    // we know result without computation:
                    // overflow in 'shl' case and 0 in 'shr' case.
                    ArithmeticOp.SHL -> if (rightValue >= java.lang.Long.BYTES) null else leftValue shl rightValue.toInt()
                    ArithmeticOp.SHR -> if (rightValue >= java.lang.Long.BYTES) 0 else leftValue shr rightValue.toInt()
                }
                result?.validValueOrNull(expectedTy)
            }
            else -> null
        }
    }

    return eval(this, 0)
}

// It returns wrong values for large types like `i128` or `usize`
// But looks like like it's enough for real cases
private val TyInteger.validValuesRange: LongRange get() = when (this) {
    TyInteger.U8 -> LongRange(0, 1L shl 8)
    TyInteger.U16 -> LongRange(0, 1L shl 16)
    TyInteger.U32 -> LongRange(0, 1L shl 32)
    TyInteger.U64 -> LongRange(0, Long.MAX_VALUE)
    TyInteger.U128 -> LongRange(0, Long.MAX_VALUE)
    TyInteger.USize -> LongRange(0, Long.MAX_VALUE)
    TyInteger.I8 -> LongRange(-(1L shl 7), (1L shl 7) - 1)
    TyInteger.I16 -> LongRange(-(1L shl 15), (1L shl 15) - 1)
    TyInteger.I32 -> LongRange(-(1L shl 31), (1L shl 31) - 1)
    TyInteger.I64 -> LongRange(Long.MIN_VALUE, Long.MAX_VALUE)
    TyInteger.I128 -> LongRange(Long.MIN_VALUE, Long.MAX_VALUE)
    TyInteger.ISize -> LongRange(Long.MIN_VALUE, Long.MAX_VALUE)
}

private fun Long.validValueOrNull(ty: TyInteger): Long? = if (this in ty.validValuesRange) this else null
