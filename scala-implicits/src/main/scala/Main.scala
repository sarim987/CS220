object PathImplicits {
import java.nio.file._

    implicit class RString(me: String){
    
        def /(other: String) = Paths.get(me, other)
        //def /(other:Path) = me.resolve(other)
        
    }
    implicit class LString(me: Path){
        def /(other:Path) = me.resolve(other)
        def write(other: String) = Files.write(me, other.getBytes())
        def read() = new String(Files.readAllBytes(me));
        def append(other: String) = Files.write(me, other.getBytes(), StandardOpenOption.APPEND)
        
    }

}

object TimeImplicits {

}