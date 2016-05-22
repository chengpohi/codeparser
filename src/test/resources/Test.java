
/*
 * Copyright (c) 1997, 2004, Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.  Oracle designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Oracle in the LICENSE file that accompanied this code.
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

package com.sun.corba.se.impl.activation;

import com.sun.corba.se.spi.activation.RepositoryPackage.ServerDef;
import com.sun.corba.se.spi.activation.ServerAlreadyRegistered;
import com.sun.corba.se.spi.activation._RepositoryImplBase;

import java.io.Serializable;
import java.util.Enumeration;

/**
 * @author Rohit Garg
 * @since JDK1.2
 */
public class RepositoryImpl extends _RepositoryImplBase
        implements Serializable {

    public int registerServer(ServerDef serverDef, int theServerId)
            throws ServerAlreadyRegistered {
        int serverId;
        DBServerDef server = null;
        System.out.println(
                "RepositoryImpl: registerServer called " +
                        "to register ServerDef " +
                        printServerDef(serverDef) +
                        " with " + ((theServerId == illegalServerId) ?
                        "a new server Id" : ("server Id " + theServerId)) +
                        " FAILED because it is already registered.");
    }

}
