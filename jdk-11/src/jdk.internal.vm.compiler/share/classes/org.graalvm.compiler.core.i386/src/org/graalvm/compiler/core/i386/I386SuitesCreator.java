/*
 * Copyright (c) 2015, Oracle and/or its affiliates. All rights reserved.
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
package org.graalvm.compiler.core.i386;

import org.graalvm.compiler.java.DefaultSuitesCreator;
import org.graalvm.compiler.lir.i386.phases.StackMoveOptimizationPhase;
import org.graalvm.compiler.lir.phases.LIRSuites;
import org.graalvm.compiler.nodes.graphbuilderconf.GraphBuilderConfiguration.Plugins;
import org.graalvm.compiler.options.OptionValues;
import org.graalvm.compiler.phases.tiers.CompilerConfiguration;

public class I386SuitesCreator extends DefaultSuitesCreator {

    public I386SuitesCreator(CompilerConfiguration compilerConfiguration, Plugins plugins) {
        super(compilerConfiguration, plugins);
    }

    @Override
    public LIRSuites createLIRSuites(OptionValues options) {
        LIRSuites lirSuites = super.createLIRSuites(options);
        if (StackMoveOptimizationPhase.Options.LIROptStackMoveOptimizer.getValue(options)) {
            /* Note: this phase must be inserted <b>after</b> RedundantMoveElimination */
            lirSuites.getPostAllocationOptimizationStage().appendPhase(new StackMoveOptimizationPhase());
        }
        return lirSuites;
    }
}
