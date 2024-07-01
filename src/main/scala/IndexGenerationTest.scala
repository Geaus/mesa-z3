import org.locationtech.geomesa.curve.Z3SFC
import org.locationtech.geomesa.curve.TimePeriod

object IndexGenerationTest extends App {

  // 输入经度、纬度、时间
  val longitude = 45.0
  val latitude = 30.0
  val time = 3600

  // 创建 Z3SFC 实例，使用默认的时间精度
  val sfc = new Z3SFC(TimePeriod.Day)

  // 生成索引
  val index = sfc.index(longitude, latitude, time)

  // 打印生成的索引
  println(s"Generated index for (lon=$longitude, lat=$latitude, time=$time): $index")

  // 反向解析索引，验证逆过程
  val (lon, lat, t) = sfc.invert(index)
  println(s"Recovered (lon=$lon, lat=$lat, time=$t) from index $index")
}
