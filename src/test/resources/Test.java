
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

    private void run(String[] args)
    {
        try {
            // parse the args and try setting the values for these
            // properties
            processArgs(args);

            ORB orb = createORB(args);

            if (orb.orbdDebugFlag)
                System.out.println( "ORBD begins initialization." ) ;

            boolean firstRun = createSystemDirs( ORBConstants.DEFAULT_DB_DIR );

            startActivationObjects(orb);

            if (firstRun) // orbd is being run the first time
                installOrbServers(getRepository(), getActivator());

            if (orb.orbdDebugFlag) {
                System.out.println( "ORBD is ready." ) ;
                System.out.println("ORBD serverid: " +
                        System.getProperty(ORBConstants.SERVER_ID_PROPERTY));
                System.out.println("activation dbdir: " +
                        System.getProperty(ORBConstants.DB_DIR_PROPERTY));
                System.out.println("activation port: " +
                        System.getProperty(ORBConstants.ORBD_PORT_PROPERTY));

                String pollingTime = System.getProperty(
                        ORBConstants.SERVER_POLLING_TIME);
                if( pollingTime == null ) {
                    pollingTime = Integer.toString(
                            ORBConstants.DEFAULT_SERVER_POLLING_TIME );
                }
                System.out.println("activation Server Polling Time: " +
                        pollingTime + " milli-seconds ");

                String startupDelay = System.getProperty(
                        ORBConstants.SERVER_STARTUP_DELAY);
                if( startupDelay == null ) {
                    startupDelay = Integer.toString(
                            ORBConstants.DEFAULT_SERVER_STARTUP_DELAY );
                }
                System.out.println("activation Server Startup Delay: " +
                        startupDelay + " milli-seconds " );
            }

            // The following two lines start the Persistent NameService
            NameServiceStartThread theThread =
                    new NameServiceStartThread( orb, dbDir );
            theThread.start( );

            orb.run();
        } catch( org.omg.CORBA.COMM_FAILURE cex ) {
            System.out.println( CorbaResourceUtil.getText("orbd.commfailure"));
            System.out.println( cex );
            cex.printStackTrace();
        } catch( org.omg.CORBA.INTERNAL iex ) {
            System.out.println( CorbaResourceUtil.getText(
                    "orbd.internalexception"));
            System.out.println( iex );
            iex.printStackTrace();
        } catch (Exception ex) {
            System.out.println(CorbaResourceUtil.getText(
                    "orbd.usage", "orbd"));
            System.out.println( ex );
            ex.printStackTrace();
        }
    }
}