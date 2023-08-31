package ru.tinkoff.oolong.mongo

import org.scalatest.funsuite.AnyFunSuite
import ru.tinkoff.oolong.PropSelectorTestMacro.prop
import ru.tinkoff.oolong.dsl.*

class PropSelectorSpec extends AnyFunSuite {

  final case class Address(
      countryCode: String,
      countryAddress: CountryAddress,
  )

  final case class CountryAddress(
      city: String,
      cityAddress: Option[CityAddress]
  )

  final case class CityAddress(
      street: String,
      building: Long,
      buildingAddress: BuildingAddress
  )

  final case class BuildingAddress(
      apartment: Int,
      apartmentAddress: Option[ApartmentAddress]
  )

  final case class ApartmentAddress(
      room: Int,
  )

  test("basic field selector") {
    val result = prop[Address](addr => addr.countryCode)

    assert(result == Some("addr", List("countryCode")))
  }
  test("long field selector") {
    val result = prop[Address](addr => addr.countryAddress.city)

    assert(result == Some("addr", List("countryAddress", "city")))
  }
  test("one optional field selector") {
    val result = prop[Address](addr => addr.countryAddress.cityAddress.!!.street)

    assert(result == Some("addr", List("countryAddress", "cityAddress", "street")))
  }
  test("repeated optional field selector") {
    val result = prop[Address](addr => addr.countryAddress.cityAddress.!!.buildingAddress.apartmentAddress.!!.room)

    assert(result == Some("addr", List("countryAddress", "cityAddress", "buildingAddress", "apartmentAddress", "room")))
  }
}
