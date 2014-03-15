package application;

import matrix.Matrix;
import matrix.Multiply;
import matrix.MyMatrixOperations;

import java.lang.reflect.Array;
import java.util.Arrays;

public class LUDecomposition implements java.io.Serializable {


    //matrix of L and U matrix in one :)
    private final double[][] LU;


    // Row and column dimensions, and pivot sign.

    private final int m, n;

    private int pivsign;


    //Вектор пивотов.

    private final int[] piv;

    public LUDecomposition(Matrix A) {

        LU =copyOf(A.items());
        m = A.row_count();
        n = A.cols();
        piv = new int[m];
        for (int i = 0; i < m; i++) {
            piv[i] = i;
        }
        pivsign = 1;
        double[] LUrowi;
        double[] LUcolj = new double[m];

        // Внешний цикл.

        for (int j = 0; j < n; j++) {

            // Make a copy of the j-th column to localize references.

            for (int i = 0; i < m; i++) {
                LUcolj[i] = LU[i][j];
            }

            // Apply previous transformations.

            for (int i = 0; i < m; i++) {
                LUrowi = LU[i];


                int kmax = Math.min(i, j);
                double s = 0.0;
                for (int k = 0; k < kmax; k++) {
                    s += LUrowi[k] * LUcolj[k];
                }

                LUrowi[j] = LUcolj[i] -= s;
            }

            // Find pivot and exchange if necessary.

            int p = j;
            for (int i = j + 1; i < m; i++) {
                if (Math.abs(LUcolj[i]) > Math.abs(LUcolj[p])) {
                    p = i;
                }
            }
            if (p != j) {
                for (int k = 0; k < n; k++) {
                    double t = LU[p][k];
                    LU[p][k] = LU[j][k];
                    LU[j][k] = t;
                }
                int k = piv[p];
                piv[p] = piv[j];
                piv[j] = k;
                pivsign = -pivsign;
            }

            // Compute multipliers.

            if (j < m & LU[j][j] != 0.0) {
                for (int i = j + 1; i < m; i++) {
                    LU[i][j] /= LU[j][j];
                }
            }
        }
    }

    /**
     * @return true if U, and hence A, is nonsingular.
     */
    public boolean isNonsingular() {
        for (int j = 0; j < n; j++) {
            if (LU[j][j] == 0)
                return false;
        }
        return true;
    }

    /**
     * return Left triangular matrix
     *
     * @return L
     */
    public Matrix getL() {
        Matrix X = new Matrix(new double[m][n]);
        double[][] L = X.items();
        for (int i = 0; i < m; i++) {
            for (int j = 0; j < n; j++) {
                if (i > j) {
                    L[i][j] = LU[i][j];
                } else if (i == j) {
                    L[i][j] = 1.0;
                } else {
                    L[i][j] = 0.0;
                }
            }
        }
        return X;
    }

    public Matrix inverseU() {
        double[][] s = new double[n][n];

        s[n - 1][n - 1] = 1 / LU[n - 1][n - 1];
        for (int i = n - 2; i >= 0; i--) {
            s[i][i] = 1 / LU[i][i];
            for (int j = n - 1; j >= i + 1; j--) {
                double alf = 0;
                for (int k = n - 1; k >= i + 1; k--) {
                    alf = alf - LU[i][k] * s[k][j];
                }
                s[i][j] = alf / LU[i][i];
            }
        }
        return new Matrix(s);
    }

    public Matrix inverseL() {
        double[][] s = new double[n][n];
        double[][] lu = new double[n][n];
        double[][] copy = getL().T().items();
        //for (iter1 <-0 until  W.row_count; iter2 <- 0 until W.cols){copyOfarray(iter1)(iter2) = W.apply(iter1,iter2)}
        for (int i1 = 0; i1 < n; i1++) {
            for (int i2=0; i2 < n;i2++) {
                lu[i1][i2] = copy[i1][i2];
            }
        }
        s[n - 1][n - 1] = 1 / lu[n - 1][n - 1];
        for (int i = n - 2; i >= 0; i--) {
            s[i][i] = 1 / lu[i][i];
            for (int j = n - 1; j >= i + 1; j--) {
                double alf = 0;
                for (int k = n - 1; k >= i + 1; k--) {
                    alf = alf -lu[i][k] * s[k][j];
                }
                s[i][j] = alf / lu[i][i];
            }
        }
        return new Matrix(s).T();
    }
    public Matrix inverseUL() {
        double[][] s = new double[n][n];
        double[][] s1 = new double[n][n];
        double[][] lu = new double[n][n];
        double[][] copy = getL().T().items();
        //for (iter1 <-0 until  W.row_count; iter2 <- 0 until W.cols){copyOfarray(iter1)(iter2) = W.apply(iter1,iter2)}
        for (int i1 = 0; i1 < n; i1++) {
            for (int i2=0; i2 < n;i2++) {
                lu[i1][i2] = copy[i1][i2];
            }
        }
        s[n - 1][n - 1] = 1 / lu[n - 1][n - 1];
        s1[n - 1][n - 1] = 1 / LU[n - 1][n - 1];
        for (int i = n - 2; i >= 0; i--) {
            s[i][i] = 1 / lu[i][i];
            s1[i][i] = 1 / LU[i][i];
            for (int j = n - 1; j >= i + 1; j--) {
                double alf = 0;
                double alf1 = 0;
                for (int k = n - 1; k >= i + 1; k--) {
                    alf = alf -lu[i][k] * s[k][j];
                    alf1 = alf1 - LU[i][k] * s1[k][j];

                }
                s[i][j] = alf / lu[i][i];
                s1[i][j] = alf1 / LU[i][i];
            }

        }

        return new Matrix(MyMatrixOperations.multiply(s1, new Matrix(s).T().items()));
    }
    /**
     * return Upper triangular matrix
     *
     * @return U
     */
    public Matrix getU() {
        Matrix X = new Matrix(new double[n][n]);
        double[][] U = X.items();
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++) {
                if (i <= j) {
                    U[i][j] = LU[i][j];
                } else {
                    U[i][j] = 0.0;
                }
            }
        }
        return X;
    }
    public static double[][] copyOf(double[][] original) {
        double[][] copy = new double[original.length][];
        for (int i = 0; i < original.length; i++) {
            copy[i] = Arrays.copyOf(original[i],original[i].length);
        }
        return copy;
    }
    private void copyArray(final double[] src, final double[] dest,
                           final int destPos) {
        System.arraycopy(src, 0, dest, 0, src.length);
    }

}