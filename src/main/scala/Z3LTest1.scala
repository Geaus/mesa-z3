import org.locationtech.geomesa.curve.Z3LSFC

object Z3LTest1 extends App {

  // 创建 Z3SFC 实例，使用默认的时间精度
  val sfc = new Z3LSFC()

  def testIndexGeneration(lon: Double, lat: Double, height: Double): Unit = {
    // 生成索引
    val index = sfc.index(lon, lat, height)
    val idxstr = index.toBinaryString

    // 打印生成的索引
    println(s"Generated index for (lon=$lon, lat=$lat, height=$height): $idxstr")

    // 反向解析索引，验证逆过程
    val (recoveredLon, recoveredLat, recoveredHeight) = sfc.invert(index)
    println(s"Recovered (lon=$recoveredLon, lat=$recoveredLat, height=$recoveredHeight) from index $index")
  }

  // 测试经度逐渐增长
  println("Testing increasing longitude:")
  for (lon <- 45.0 to 50.0 by 1.0) {
    testIndexGeneration(lon, 30.0, 3601.0)
  }

  // 测试纬度逐渐增长
  println("Testing increasing latitude:")
  for (lat <- 30.0 to 35.0 by 1.0) {
    testIndexGeneration(45.0, lat, 3601.0)
  }

  // 测试高度逐渐增长
  println("Testing increasing height:")
  for (height <- 3601.0 to 3606.0 by 1.0) {
    testIndexGeneration(45.0, 30.0, height)
  }
}
