package build.tools.logutil;

import java.io.PrintWriter ;
import java.io.Writer ;
import java.io.OutputStream ;
import java.io.BufferedWriter ;
import java.io.OutputStreamWriter ;
import java.util.StringTokenizer ;

public class Vargs extends PrintWriter {
    public void printMsg( String msg, Object... data )
    {
    }
    private void updateIndentString()
    {
        int size = level * indentWidth ;
        StringBuffer sbuf = new StringBuffer( size ) ;
        for (int ctr = 0; ctr<size; ctr++ )
            sbuf.append( " " ) ;
        indentString = sbuf.toString() ;
    }
}
