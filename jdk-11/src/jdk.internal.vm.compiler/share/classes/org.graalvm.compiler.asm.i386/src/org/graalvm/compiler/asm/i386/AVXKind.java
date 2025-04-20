/*
 * Copyright (c) 2013, 2018, Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.
 *
 * This code is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * version 2 for more details (a copy is included in the LICENSE file that
 * accompanied this code).
 *
 * You should have received a copy of the GNU General Public License version
 * 2 along with this work; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 * or visit www.oracle.com if you need additional information or have any
 * questions.
 */
package org.graalvm.compiler.asm.i386;

import static org.graalvm.compiler.asm.i386.AVXKind.AVXSize.DWORD;
import static org.graalvm.compiler.asm.i386.AVXKind.AVXSize.QWORD;
import static org.graalvm.compiler.asm.i386.AVXKind.AVXSize.XMM;
import static org.graalvm.compiler.asm.i386.AVXKind.AVXSize.YMM;

import jdk.vm.ci.meta.Value;
import org.graalvm.compiler.debug.GraalError;

import jdk.vm.ci.i386.I386Kind;

/**
 * Helper methods for dealing with AVX and SSE {@link I386Kind I386Kinds}.
 */
public final class AVXKind {

    public enum AVXSize {
        DWORD,
        QWORD,
        XMM,
        YMM;

        public int getBytes() {
            switch (this) {
                case DWORD:
                    return 4;
                case QWORD:
                    return 8;
                case XMM:
                    return 16;
                case YMM:
                    return 32;
                default:
                    return 0;
            }
        }
    }

    private AVXKind() {
    }

    public static AVXSize getRegisterSize(Value a) {
        I386Kind kind = (I386Kind) a.getPlatformKind();
        if (kind.isXMM()) {
            return getRegisterSize(kind);
        } else {
            return XMM;
        }
    }

    public static AVXSize getDataSize(I386Kind kind) {
        assert kind.isXMM() : "unexpected kind " + kind;
        switch (kind.getSizeInBytes()) {
            case 4:
                return DWORD;
            case 8:
                return QWORD;
            case 16:
                return XMM;
            case 32:
                return YMM;
            default:
                throw GraalError.shouldNotReachHere("unsupported kind: " + kind);
        }
    }

    public static AVXSize getRegisterSize(I386Kind kind) {
        assert kind.isXMM() : "unexpected kind " + kind;
        if (kind.getSizeInBytes() > 16) {
            return YMM;
        } else {
            return XMM;
        }
    }

    public static I386Kind changeSize(I386Kind kind, AVXSize newSize) {
        return getAVXKind(kind.getScalar(), newSize);
    }

    public static I386Kind getAVXKind(I386Kind base, AVXSize size) {
        for (I386Kind ret : I386Kind.values()) {
            if (ret.getScalar() == base && ret.getSizeInBytes() == size.getBytes()) {
                return ret;
            }
        }
        throw GraalError.shouldNotReachHere(String.format("unsupported vector kind: %s x %s", size, base));
    }

    public static I386Kind getAVXKind(I386Kind base, int length) {
        for (I386Kind ret : I386Kind.values()) {
            if (ret.getScalar() == base && ret.getVectorLength() == length) {
                return ret;
            }
        }
        throw GraalError.shouldNotReachHere(String.format("unsupported vector kind: %d x %s", length, base));
    }
}
