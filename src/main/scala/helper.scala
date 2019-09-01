import java.nio.file.{Files, Paths}
import java.nio.charset.StandardCharsets

object helper {

  def findsql(lines: String) =
  {
    val filePattern = """<SQL><SelectStatement.+<!\[CDATA\[([.\S\s]+?)\]\]>""".r
    val sqqls = for (m <- filePattern.findAllMatchIn(lines)) yield m.group(1)
    sqqls
  }

  //replace comments in sql
  def repcmmntsql(str: String) =
  {
    val commentsrepl = """(--.*)|(((/\*)+?[\w\W]+?(\*/)+))""".r
    val sqlswithnocomments = for (m <- commentsrepl.replaceAllIn(str,"")) yield m
    sqlswithnocomments
  }

  //retrieve tables from sqls
  def gettablessql(str: String) =
  {
    val selectfrom = """(?i)(?<!leading 0 )FROM[\[&"#\s]+([.\w]+[.\S\s]*?)(;|where|left|right|join|inner|\(|\)|union|0|\\|select|group by|order by)""".r   //(?i)FROM[\s]+([.\w]+[.\S\s]*?)(;|where|left|right|join|inner|\(|\)|union|0|\\|select)
    val selectjoin = """(?i)JOIN[\s]+([.\w]+)""".r

    val tablelistsfrom = for (m <- selectfrom.findAllMatchIn(str)) yield m.group(1)
    val tablelistsjoin = for (m <- selectjoin.findAllMatchIn(str)) yield m.group(1)

    tablelistsfrom ++ tablelistsjoin
  }

  def putsqlinfile(sqqls: Iterator[String],filewithnoprefx: String,dirpath: String) = {
    val commentfiltered = sqqls.mkString("/////////////////////==============================================/////////////////")
    Files.write(Paths.get(dirpath + filewithnoprefx + "_SQLs.txt"), commentfiltered.getBytes(StandardCharsets.UTF_16))
    println("Generated "+ dirpath+filewithnoprefx+"_SQLs.txt")
  }

  def puttablesinfiles(sqqls: Iterator[String],filewithnoprefx: String,dirpath: String) = {

    val commentfiltered = sqqls.map(x => repcmmntsql(x))
    //get list of tables
    val tablelist = commentfiltered.flatMap(x => gettablessql(x))
    val tablles = tablelist.map(_.trim).toList.distinct.mkString("\n")
    Files.write(Paths.get(dirpath + filewithnoprefx + "_tables.txt"), tablles.getBytes(StandardCharsets.UTF_16))
    println("Generated "+ dirpath+filewithnoprefx+"_tables.txt")
  }

  def main(args: Array[String]): Unit = {
    try {
    val Path = args(0)              //dir path
    val fileName = args(1)          //filename
    val filewithnoprefx = args(1).substring(0,args(1).indexOf("."))
    val source = scala.io.Source.fromFile(Path + fileName, "ISO-8859-1")
    val lines = source.getLines.mkString("\n")

    putsqlinfile(findsql(lines),filewithnoprefx,args(0))
    puttablesinfiles(findsql(lines),filewithnoprefx,args(0))
    source.close()
    } catch {
          case e: Exception => println("Error: " + e)
    }
  }
}
