def orderDFByAll(df: DataFrame): DataFrame = {
    var cols = scala.collection.mutable.ArrayBuffer.empty[org.apache.spark.sql.Column]
    df.schema.fields.foreach(x => {
      cols += col(x.name)
    })
    df.orderBy(cols: _*)
  }
