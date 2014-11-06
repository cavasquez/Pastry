package com.simplepastry

object Main 
{
  def main(args: Array[String])
  {
    val boot = new BootStrapper(10, 16, 16, 100)
    boot.start
  }
} 