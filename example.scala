import com.akarthik.lsh.LSH
import scala.io.Source;


object Example {
    def main(args:Array[String]) {
        val lines = Source.fromFile(args(0)).getLines
        .map(line => line.split("\\s+").mkString(" "))
        .map(line => line.toLowerCase)
        .toIndexedSeq
        .zipWithIndex

        val string = "The brown fox jumped over the lawn"

        val lsh = new LSH(shingleLength=3,
            minHashLength = 100,numberBands=10,lines,threshold = 0.8);

        lsh.createHash();
        lsh.findSimilar(string);
    }
}

