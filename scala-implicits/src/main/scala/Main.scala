object PathImplicits {
import java.nio.file._

    implicit class RString(me: String){
        def /(other: String) = Paths.get(me, other)        
    }
    implicit class RPath(me: Path){
        def /(other:Path) = me.resolve(other)
        def write(other: String) = Files.write(me, other.getBytes())
        def read() = new String(Files.readAllBytes(me));
        def append(other: String) = Files.write(me, other.getBytes(), StandardOpenOption.APPEND)        
    }
}

object TimeImplicits {
import java.time.LocalDate;
    implicit class IntDate(me: Int){
        def jan() = LocalDate.of(2018, 1, me)
        def feb() = LocalDate.of(2018, 2, me)
        def mar() = LocalDate.of(2018, 3, me)
        def apr() = LocalDate.of(2018, 4, me)
        def may() = LocalDate.of(2018, 5, me)
        def jun() = LocalDate.of(2018, 6, me)
        def jul() = LocalDate.of(2018, 7, me)
        def aug() = LocalDate.of(2018, 8, me)
        def sep() = LocalDate.of(2018, 9, me)
        def oct() = LocalDate.of(2018, 10, me)
        def nov() = LocalDate.of(2018, 11, me)
        def dec() = LocalDate.of(2018, 12, me) 
        
        def jan(other:Int) = LocalDate.of(other, 1, me)
        def feb(other:Int) = LocalDate.of(other, 2, me)
        def mar(other:Int) = LocalDate.of(other, 3, me)
        def apr(other:Int) = LocalDate.of(other, 4, me)
        def may(other:Int) = LocalDate.of(other, 5, me)
        def jun(other:Int) = LocalDate.of(other, 6, me)
        def jul(other:Int) = LocalDate.of(other, 7, me)
        def aug(other:Int) = LocalDate.of(other, 8, me)
        def sep(other:Int) = LocalDate.of(other, 9, me)
        def oct(other:Int) = LocalDate.of(other, 10, me)
        def nov(other:Int) = LocalDate.of(other, 11, me)
        def dec(other:Int) = LocalDate.of(other, 12, me)  

        def days() = (me, "days")
        def months() = (me, "months")
        def years() = (me, "years")
    }

    implicit class AddDays(day: LocalDate){
        def +(tup:(Int, String)) = {
            if(tup._2.equals("days")){ day.plusDays(tup._1)}
            else if(tup._2.equals("months")){ day.plusMonths(tup._1)}
            else if(tup._2.equals("years")){ day.plusYears(tup._1)}
            else throw new IllegalArgumentException("tuple 2 should be days, months, or years")
        }

    }
}