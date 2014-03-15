package application;

import org.netlib.lapack.DGEQRF;

import java.util.LinkedList;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: Aleksandr
 * Date: 21.11.12
 * Time: 23:40
 * All rights recieved.(c)
 */
public class QR {

    public static List<double[][]> qr(double[][] A) {
        /* A( LDA, * ), ALPHAI( * ), ALPHAR( * ),
     $                   B( LDB, * ), BETA( * ), VL( LDVL, * ),
     $                   VR( LDVR, * ), WORK( * )*/


        int n = A.length;


        double[] w = new double[n];


        int lwork = Math.max(1, 8 * n);
        double[] work = new double[lwork];

        org.netlib.util.intW info = new org.netlib.util.intW(0);
        DGEQRF.DGEQRF(n, n, A, w, work, lwork, info);


        System.out.println("on return info = " + info.val);
        double[][] res = new double[1][];
        res[0] = w;


        List result =  new LinkedList();
        result.add(A);

        result.add(res);
        return result;
    }
}
