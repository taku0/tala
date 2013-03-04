/* 
 * Copyright 2013 taku0 ( https://github.com/taku0 )
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import scala.reflect.runtime.{universe => ru}
import ru._

import com.hp.hpl.jena.rdf.model.ModelFactory
import com.hp.hpl.jena.rdf.model.Model
import com.hp.hpl.jena.rdf.model._

import java.io.FileInputStream
import java.util.NoSuchElementException

import scala.collection.mutable
import scala.collection.JavaConverters._





case class Person(name: String, age: Int, address: Address)
case class Address(country: String, city: String)

object Main extends App {
  val reader = new TalaReader

  println(reader.readFromFile[Person]("data.ttl"))
}

class TalaReader {
  def defaultPrefix = "http://example.org/#" // TODO 固定でなくす
  def rdfVocabularyPrefix = "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
  def typeProperty(model: Model) =
    model.createProperty(rdfVocabularyPrefix, "type")

  def readFromFile[A: TypeTag](filename: String): A = {
    val model = ModelFactory.createDefaultModel

    // TODO 暗黙のプレフィックスを追加する。
    
    model.read(new FileInputStream(filename), "http://example.org", "TURTLE")

    getInstance(model)
  }

  def getInstance[A: TypeTag](model: Model): A = {
    val typeTag = ru.typeTag[A]
    val typeName = typeTag.tpe.typeSymbol.fullName
    val property = typeProperty(model)
    val objectNode = model.createResource(defaultPrefix + typeName)
    val subjects = model.listResourcesWithProperty(property, objectNode).toList.asScala.toSeq

    if (subjects.isEmpty) {
      throw new NoSuchElementException(s"type #{name} not found")
    }

    val subject = subjects.head

    getInstance(model, subject, typeTag.tpe)
  }

  def getInstance[A](model: Model,
                     resource: Resource,
                     typeObject: ru.Type,
                     cache: mutable.Map[Resource, Any] =
                       mutable.Map.empty[Resource, Any],
                     pendingResources: Set[Resource] =
                       Set.empty[Resource]): A = {
    if (pendingResources.contains(resource)) {
      // TODO 適切な例外にする
      throw new RuntimeException("cyclic dependency detected")
    }

    cache.getOrElseUpdate(resource,
                          doGetInstance[A](model,
                                           resource,
                                           typeObject,
                                           cache,
                                           pendingResources + resource)).asInstanceOf[A]
  }

  def doGetInstance[A](model: Model,
                       resource: Resource,
                       typeObject: ru.Type,
                       cache: mutable.Map[Resource, Any],
                       pendingResources: Set[Resource]): A = {
    val mirror = ru.runtimeMirror(Thread.currentThread.getContextClassLoader)
    val constructor = typeObject.declaration(ru.nme.CONSTRUCTOR).asMethod
    val parameters = constructor.paramss.flatten

    def getValue[A](node: RDFNode, typeObject: ru.Type): A = {
      // リテラルだったらその値
      // そうでなければgetInstanceの再帰呼び出し
      node match {
        case literal: com.hp.hpl.jena.rdf.model.Literal =>
          literal.getValue.asInstanceOf[A]
        case resource: Resource =>
          getInstance[A](model, resource, typeObject, cache, pendingResources)
      }
    }

    val arguments = for {
      parameter <- parameters
    } yield {
      val name = parameter.name
      val typeSignature = parameter.typeSignature
      // メモ: ↓パラメータは含まない。
      val typeName = parameter.typeSignature.typeSymbol.fullName
      val property = model.createProperty(defaultPrefix, name.toString)
      val objectNodes = model.listObjectsOfProperty(resource, property).toList.asScala

      if (objectNodes.isEmpty) {
        // TODO 適切な例外にする
        throw new RuntimeException("missing property " + name)
      } else if (objectNodes.size == 1) {
        val objectNode = objectNodes.head

        if (isAcceptable(model, typeSignature, objectNode)) {
          getValue(objectNode, typeSignature)
        } else if (isAcceptable(model, typeSignature, objectNodes)) {
          // val companion: {def empty: Iterable[A]} = getCompanion(mirror, typeSignature).asInstanceOf[{def empty: Iterable[A]}]
          
          // companion.empty + getValue(objectNode)
          null // TODO
        }
      } else {
        // val companion: {def empty: Iterable[A]} = getCompanion(mirror, typeSignature).asInstanceOf[{def empty: Iterable[A]}]
          
        // companion.empty ++ objectNodes.map(getValue _)
        null // TODO
      }
    }

    mirror.reflectClass(typeObject.typeSymbol.asClass).reflectConstructor(constructor).apply(arguments: _*).asInstanceOf[A]
  }

  def isAcceptable(model: Model, typeSignature: ru.Type, nodes: Iterable[RDFNode]) = {
    if (isCollectionType(typeSignature)) {
      val typeArguments = typeSignature.asInstanceOf[TypeRef].args

      if (typeArguments.size != 1) {
        // TODO 適切な例外にする
        throw new RuntimeException("cannot detect type arguments")
      }

      val typeArgument = typeArguments.head

      true // TODO typeSignatureがコレクションであり、要素の型が合う
    } else {
      // TODO 適切な例外にする
      throw new RuntimeException("too many values for scalar property")
    }
  }

  def isAcceptable(model: Model, typeSignature: ru.Type, node: RDFNode) = {
    val types = getTypes(model, node)

    types.isEmpty || types.exists {typeOfNode =>
      // xsdの型の場合
      // デフォルトネームスペース以下の場合
      true // TODO
    }
  }

  def getTypes(model: Model, node: RDFNode) = {
    node match {
      case literal: com.hp.hpl.jena.rdf.model.Literal =>
        if (literal.getDatatype == null) {
          Set.empty
        } else {
          Set(model.createResource(literal.getDatatypeURI))
        }
      case resource: Resource =>
        model.listObjectsOfProperty(resource, typeProperty(model)).toList.asScala
    }
  }

  def getCompanion(mirror: Mirror, typeSignature: ru.Type) = {
    mirror.reflectModule(typeSignature.typeSymbol.asClass.companionSymbol.asModule).instance
  }

  def isCollectionType(typeSignature: ru.Type) = {
    false // TODO
  }
}


  // val mirror = ru.runtimeMirror(Thread.currentThread.getContextClassLoader)

  // val personType = typeOf[Person]

  // personType match {
  //   case typeRef @ TypeRef(prefix, symbol, typeArgs) =>
  //     val constructor = typeRef.declaration(ru.nme.CONSTRUCTOR)

  //     if (constructor.isMethod) {
  //       println(constructor.asMethod.paramss)
  //     }
  //     // mirror reflectClass personType.typeSymbol.asClass reflectConstructor constructor
  // }

  // val model = ModelFactory.createDefaultModel
  // 
  // model.read(new FileInputStream("data.ttl"), "http://example.org", "TURTLE")
  // 
  // println(model)

