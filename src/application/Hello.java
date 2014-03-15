package application;

import matrix.Complex;
import matrix.Matrix;
import matrix.MatrixComplex;
import scala.Tuple2;

import java.util.InputMismatchException;
import java.util.Scanner;




/**
 * Created with IntelliJ IDEA.
 * User: Aleksandr
 * Date: 20.11.12
 * Time: 15:10
 * All rights recieved.(c)
 */
public class Hello {
    public static void main(String[] args) {
        boolean  flag  = true;

        while(flag)    {
        try {

            MatrixComplex e = new MatrixComplex(new Complex[1][]);
            MatrixComplex v = new MatrixComplex(new Complex[1][]);
            Tuple2<MatrixComplex, MatrixComplex> t = new Tuple2<MatrixComplex, MatrixComplex>(e, v);

            System.out.println("Hello! Write 0 if you want to read from file matrixes A.txt and B.txt else 1");
            Scanner in = new Scanner(System.in);
            int answer = in.nextInt();
            System.out.println("Write the number of eigenvectors(m<<n, where n is a size of A)");
            int num = in.nextInt();
            if (answer == 0) {
                Matrix b = Matrix.fromFile("B.txt");
                Matrix a = Matrix.fromFile("A.txt");
                long time1 = System.currentTimeMillis();


                t = application.IterateEigenComplex.subspaceIter(a, b, -0.01, 1e-10, num);
                long time2 = System.currentTimeMillis();
                System.out.println("Time of computing " + (time2 - time1) + " ms"
                        + "\n Approximate = " + Math.pow(a.row_count() * 2, a.row_count() * 2.0) / 100 + "ms");
                t._1().saveToFile("outputE.txt");
                t._2().saveToFile("outputV.txt");

            } else {
                // Reads a integer from the console
                // and stores into age variable
                System.out.println("Write a dimension of generated symmetric matrix A");

                int dim = in.nextInt();
                Generator g = new Generator(dim, 3);
                Matrix m = g.generateMatrix();
                //Matrix n = new Generator(5, 1).generateMatrix();
                Matrix n = g.eye(dim);


                long time1 = System.currentTimeMillis();
               // t = IterateEigen.subspaceIter(m, n, 0.1, 1e-10, num);
                long time2 = System.currentTimeMillis();
                if(dim < 150){
                System.out.println("Time of computing " + (time2 - time1) + " ms"
                        + "\n Approximate = " + (m.row_count() * 2* n.row_count()*dim * 2.0) / 10+ "ms"); }
                else { System.out.println("Time of computing " + (time2 - time1) + " ms"
                        + "\n Approximate = " + (m.row_count() * 2* n.row_count()*dim * 2.0) / 1000+ "ms"); }
                m.saveToFile("generatedMatrixA.txt");
                n.saveToFile("genMatrixB.txt");

                t._1().saveToFile("outputE.txt");
                t._2().saveToFile("outputV.txt");

            }


            System.out.println("Write 1 for exit");
            int dim = in.nextInt();
            if(dim == 1) {flag = false;
              //IterateEigen.closes();
              }
        }

        catch (InputMismatchException e) {
            //e.printStackTrace();
            System.out.println("Write an integer from 0 to 10000");
        }
       catch (Exception e){ System.out.println(e.getLocalizedMessage());}
    }     }

}
