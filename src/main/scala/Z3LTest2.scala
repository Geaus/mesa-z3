import org.locationtech.geomesa.curve.Z3LSFC

object Z3LTest2 extends App {

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
  println("Testing special points")
  testIndexGeneration(-180.0, -90.0, 3000.0)



}
