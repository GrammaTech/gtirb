/**
 * A sample test.
 *
 * <p>Open a gtirb file and read its contents using the GTIRB Java API.
 */
import com.grammatech.gtirb.IR;
import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;

public class testIrSanity {

    public static void main(String[] args) {

        if (args.length < 1) {
            System.err.println("No GTIRB file specified.");
            System.err.println("test failed.");
            return;
        }

        InputStream inputStream;
        boolean loadReturned;
        //IR ir = new IR();
        String fileName = args[0];
        File inputFile = new File(fileName);
        try {
            inputStream = new FileInputStream(inputFile);
            IR ir  = IR.loadFile(inputStream);
            //loadReturned = (ir == null);
            if (ir == null) {
                loadReturned = false;
            } else {
                loadReturned = true;
            }
            inputStream.close();
        } catch (Exception e) {
            System.out.println("Unable to parse " + fileName + ".");
            System.err.println("test failed.");
            return;
        }

        if (loadReturned != true) {
            System.out.println("Unable to load " + fileName + ".");
            System.err.println("test failed.");
            return;
        }

        System.err.println("test OK.");
        return;
    }
}
