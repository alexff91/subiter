package application;

import matrix.Complex;
import org.netlib.lapack.DGGEV;
import org.netlib.lapack.SGGEV;

import java.util.LinkedList;
import java.util.List;

/**
 * Created with IntelliJ IDEA.
 * User: Aleksandr
 * Date: 19.11.12
 * Time: 23:25
 * All rights recieved.(c)
 * *  -- LAPACK driver routine (version 3.2) --
 *  -- LAPACK is a software package provided by Univ. of Tennessee,    --
 *  -- Univ. of California Berkeley, Univ. of Colorado Denver and NAG Ltd..--
 *     November 2006
 *
 *     .. Scalar Arguments ..
 CHARACTER          JOBVL, JOBVR
 INTEGER            INFO, LDA, LDB, LDVL, LDVR, LWORK, N
 *     ..
 *     .. Array Arguments ..
 REAL               A( LDA, * ), ALPHAI( * ), ALPHAR( * ),
 $                   B( LDB, * ), BETA( * ), VL( LDVL, * ),
 $                   VR( LDVR, * ), WORK( * )
 *     ..
 *
 *  Purpose
 *  =======
 *
 *  SGGEV computes for a pair of N-by-N real nonsymmetric matrices (A,B)
 *  the generalized eigenvalues, and optionally, the left and/or right
 *  generalized eigenvectors.
 *
 *  A generalized eigenvalue for a pair of matrices (A,B) is a scalar
 *  lambda or a ratio alpha/beta = lambda, such that A - lambda*B is
 *  singular. It is usually represented as the pair (alpha,beta), as
 *  there is a reasonable interpretation for beta=0, and even for both
 *  being zero.
 *
 *  The right eigenvector v(j) corresponding to the eigenvalue lambda(j)
 *  of (A,B) satisfies
 *
 *                   A * v(j) = lambda(j) * B * v(j).
 *
 *  The left eigenvector u(j) corresponding to the eigenvalue lambda(j)
 *  of (A,B) satisfies
 *
 *                   u(j)**H * A  = lambda(j) * u(j)**H * B .
 *
 *  where u(j)**H is the conjugate-transpose of u(j).
 *
 *
 *  Arguments
 *  =========
 *
 *  JOBVL   (input) CHARACTER*1
 *          = 'N':  do not compute the left generalized eigenvectors;
 *          = 'V':  compute the left generalized eigenvectors.
 *
 *  JOBVR   (input) CHARACTER*1
 *          = 'N':  do not compute the right generalized eigenvectors;
 *          = 'V':  compute the right generalized eigenvectors.
 *
 *  N       (input) INTEGER
 *          The order of the matrices A, B, VL, and VR.  N >= 0.
 *
 *  A       (input/output) REAL array, dimension (LDA, N)
 *          On entry, the matrix A in the pair (A,B).
 *          On exit, A has been overwritten.
 *
 *  LDA     (input) INTEGER
 *          The leading dimension of A.  LDA >= max(1,N).
 *
 *  B       (input/output) REAL array, dimension (LDB, N)
 *          On entry, the matrix B in the pair (A,B).
 *          On exit, B has been overwritten.
 *
 *  LDB     (input) INTEGER
 *          The leading dimension of B.  LDB >= max(1,N).
 *
 *  ALPHAR  (output) REAL array, dimension (N)
 *  ALPHAI  (output) REAL array, dimension (N)
 *  BETA    (output) REAL array, dimension (N)
 *          On exit, (ALPHAR(j) + ALPHAI(j)*i)/BETA(j), j=1,...,N, will
 *          be the generalized eigenvalues.  If ALPHAI(j) is zero, then
 *          the j-th eigenvalue is real; if positive, then the j-th and
 *          (j+1)-st eigenvalues are a complex conjugate pair, with
 *          ALPHAI(j+1) negative.
 *
 *          Note: the quotients ALPHAR(j)/BETA(j) and ALPHAI(j)/BETA(j)
 *          may easily over- or underflow, and BETA(j) may even be zero.
 *          Thus, the user should avoid naively computing the ratio
 *          alpha/beta.  However, ALPHAR and ALPHAI will be always less
 *          than and usually comparable with norm(A) in magnitude, and
 *          BETA always less than and usually comparable with norm(B).
 *
 *  VL      (output) REAL array, dimension (LDVL,N)
 *          If JOBVL = 'V', the left eigenvectors u(j) are stored one
 *          after another in the columns of VL, in the same order as
 *          their eigenvalues. If the j-th eigenvalue is real, then
 *          u(j) = VL(:,j), the j-th column of VL. If the j-th and
 *          (j+1)-th eigenvalues form a complex conjugate pair, then
 *          u(j) = VL(:,j)+i*VL(:,j+1) and u(j+1) = VL(:,j)-i*VL(:,j+1).
 *          Each eigenvector is scaled so the largest component has
 *          abs(real part)+abs(imag. part)=1.
 *          Not referenced if JOBVL = 'N'.
 *
 *  LDVL    (input) INTEGER
 *          The leading dimension of the matrix VL. LDVL >= 1, and
 *          if JOBVL = 'V', LDVL >= N.
 *
 *  VR      (output) REAL array, dimension (LDVR,N)
 *          If JOBVR = 'V', the right eigenvectors v(j) are stored one
 *          after another in the columns of VR, in the same order as
 *          their eigenvalues. If the j-th eigenvalue is real, then
 *          v(j) = VR(:,j), the j-th column of VR. If the j-th and
 *          (j+1)-th eigenvalues form a complex conjugate pair, then
 *          v(j) = VR(:,j)+i*VR(:,j+1) and v(j+1) = VR(:,j)-i*VR(:,j+1).
 *          Each eigenvector is scaled so the largest component has
 *          abs(real part)+abs(imag. part)=1.
 *          Not referenced if JOBVR = 'N'.
 *
 *  LDVR    (input) INTEGER
 *          The leading dimension of the matrix VR. LDVR >= 1, and
 *          if JOBVR = 'V', LDVR >= N.
 *
 *  WORK    (workspace/output) REAL array, dimension (MAX(1,LWORK))
 *          On exit, if INFO = 0, WORK(1) returns the optimal LWORK.
 *
 *  LWORK   (input) INTEGER
 *          The dimension of the array WORK.  LWORK >= max(1,8*N).
 *          For good performance, LWORK must generally be larger.
 *
 *          If LWORK = -1, then a workspace query is assumed; the routine
 *          only calculates the optimal size of the WORK array, returns
 *          this value as the first entry of the WORK array, and no error
 *          message related to LWORK is issued by XERBLA.
 *
 *  INFO    (output) INTEGER
 *          = 0:  successful exit
 *          < 0:  if INFO = -i, the i-th argument had an illegal value.
 *          = 1,...,N:
 *                The QZ iteration failed.  No eigenvectors have been
 *                calculated, but ALPHAR(j), ALPHAI(j), and BETA(j)
 *                should be correct for j=INFO+1,...,N.
 *          > N:  =N+1: other than QZ iteration failed in SHGEQZ.
 *                =N+2: error return from STGEVC.
 */
public class QZ {
    public static List<float[][]> qz(float[][] A, float[][] B) {

        String jobz = new String("V");

        int n = A.length;
        float[][] X = new float[n][n];
        float[][] Y = new float[n][n];

        float[] w = new float[n];
        float[] alphar = new float[n];
        float[] alphai = new float[n];
        float[] beta = new float[n];
        int lwork = Math.max(1, 8 * n);
        float[] work = new float[lwork];

        org.netlib.util.intW info = new org.netlib.util.intW(0);
        SGGEV.SGGEV(jobz, jobz, n, A, B, alphar, alphai, beta, X, Y, work, lwork, info);


        System.out.println("on return info = " + info.val);

        float[][] res = new float[3][];
        res[0] = alphar;
        res[1] = alphai;
        res[2]  = beta;
        List result =  new LinkedList();
        result.add(X);
        result.add(Y);
        result.add(res);
        return result;
    }
    public static List<double[][]> qzIM(double[][] A, double[][] B) throws Exception {

        String jobz = new String("V");

        int n = A.length;
        double[][] X = new double[n][n];
        double[][] Y = new double[n][n];

        double[] w = new double[n];
        double[] alphar = new double[n];
        double[] alphai = new double[n];
        double[] beta = new double[n];
        int lwork = Math.max(1, 8 * n);
        double[] work = new double[lwork];

        org.netlib.util.intW info = new org.netlib.util.intW(0);
        DGGEV.DGGEV(jobz, jobz, n, A, B, alphar, alphai, beta, X, Y, work, lwork, info);
        System.out.println(info.val);
        //if(info.val!= 0)throw new Exception("QZ iteration failed!");

        double[][] res = new double[3][];
        res[0] = alphar;
        res[1] = alphai;
        res[2]  = beta;
        List result =  new LinkedList();
        result.add(Y);
        result.add(X);
        result.add(res);
        return result;
    }
    public static List<double[][]> qz(double[][] A, double[][] B) throws Exception {

        String jobz = new String("V");

        int n = A.length;
        double[][] X = new double[n][n];
        double[][] Y = new double[n][n];

        double[] w = new double[n];
        double[] alphar = new double[n];
        double[] alphai = new double[n];
        double[] beta = new double[n];
        int lwork = Math.max(1, 8 * n);
        double[] work = new double[lwork];

        org.netlib.util.intW info = new org.netlib.util.intW(0);
        DGGEV.DGGEV(jobz, jobz, n, A, B, alphar, alphai, beta, X, Y, work, lwork, info);
        System.out.println(info.val);
        //if(info.val!= 0)throw new Exception("QZ iteration failed!");

        double[][] res = new double[3][];
        res[0] = alphar;
        res[1] = alphai;
        res[2]  = beta;
        List result =  new LinkedList();
        result.add(X);
        result.add(Y);
        result.add(res);
        return result;
    }
}
