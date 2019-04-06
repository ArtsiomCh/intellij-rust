/*
 * Use of this source code is governed by the MIT license that can be
 * found in the LICENSE file.
 */

package org.rust.lang.core.psi.ext

import org.rust.lang.core.psi.RsArrayType
import org.rust.lang.core.types.ty.TyInteger

val RsArrayType.isSlice: Boolean get() = stub?.isSlice ?: (expr == null)

val RsArrayType.arraySize: Long? get() = expr?.calculate(TyInteger.USize)
