

DF1.withColumn("rank",
      rank().over(Window.partitionBy("A").orderBy($"score".desc)))
      .filter($"rank" === 1)
      .drop("rank").as[Schema]
