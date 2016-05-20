/*
 *
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
 *
 */

package com.sun.corba.se.impl.activation;

import java.io.File;
import java.util.Properties;

import org.omg.CORBA.INITIALIZE;
import org.omg.CORBA.INTERNAL;
import org.omg.CORBA.CompletionStatus;
import org.omg.CosNaming.NamingContext;
import org.omg.PortableServer.POA;

import com.sun.corba.se.pept.transport.Acceptor;

import com.sun.corba.se.spi.activation.Repository;
import com.sun.corba.se.spi.activation.RepositoryPackage.ServerDef;
import com.sun.corba.se.spi.activation.Locator;
import com.sun.corba.se.spi.activation.LocatorHelper;
import com.sun.corba.se.spi.activation.Activator;
import com.sun.corba.se.spi.activation.ActivatorHelper;
import com.sun.corba.se.spi.activation.ServerAlreadyRegistered;
import com.sun.corba.se.spi.legacy.connection.LegacyServerSocketEndPointInfo;
import com.sun.corba.se.spi.transport.SocketInfo;
import com.sun.corba.se.spi.orb.ORB;

import com.sun.corba.se.impl.legacy.connection.SocketFactoryAcceptorImpl;
import com.sun.corba.se.impl.naming.cosnaming.TransientNameService;
import com.sun.corba.se.impl.naming.pcosnaming.NameService;
import com.sun.corba.se.impl.orbutil.ORBConstants;
import com.sun.corba.se.impl.orbutil.CorbaResourceUtil;
import com.sun.corba.se.impl.transport.SocketOrChannelAcceptorImpl;

/**
 *
 * @author      Rohit Garg
 * @since       JDK1.2
 */
public class ORBD
{

    protected ORB createORB(String[] args)
    {
        Properties props = System.getProperties();

        // For debugging.
        //props.put( ORBConstants.DEBUG_PROPERTY, "naming" ) ;
        //props.put( ORBConstants.DEBUG_PROPERTY, "transport,giop,naming" ) ;

        props.put( ORBConstants.SERVER_ID_PROPERTY, "1000" ) ;
        props.put( ORBConstants.PERSISTENT_SERVER_PORT_PROPERTY,
                props.getProperty( ORBConstants.ORBD_PORT_PROPERTY,
                        Integer.toString(
                                ORBConstants.DEFAULT_ACTIVATION_PORT ) ) ) ;

        // See Bug 4396928 for more information about why we are initializing
        // the ORBClass to PIORB (now ORBImpl, but should check the bugid).
        props.put("org.omg.CORBA.ORBClass",
                "com.sun.corba.se.impl.orb.ORBImpl");

        return (ORB) ORB.init(args, props);
    }
}

