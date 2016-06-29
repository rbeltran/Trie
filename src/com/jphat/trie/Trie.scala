package com.jphat.trie

import scala.io.Source


abstract class Node
case class LeafNode( text: String ) extends Node
case class FullNode( text: String, left: Node, right: Node ) extends Node
case class LeftNode( text: String, left: Node ) extends Node
case class RightNode( text: String, right: Node ) extends Node


object Trie extends App {

  val filename = "wordlist.txt"

  var tree: Node = null
  
  def loadTrie( filename: String ) {
    for( line <- Source.fromPath( filename ).getLine()) {
      tree = insert( tree, line )
    }
  }
  
  def insert( tree: Node, word:String ) : Node = {
    tree match {
      case null => LeafNode( word )
      case LeafNode( text ) => if ( word > text ) {
        LeftNode( text, LeafNode( word ))
       } else {
         RightNode( text, LeafNode( word ))
       }
      case LeftNode( text, left ) => if( word > text ) {
        LeftNode( text, LeftNode( word, left))
      } else {
        FullNode( text, left, LeafNode( word ))
      }
      case RightNode( text, right ) => if( word > text ) {
        FullNode( text, LeafNode( word ), right)
      } else {
        RightNode( word, RightNode( text, right ))
      }
      
      case FullNode( text, left, right) => if( word > text) {
        FullNode( text, insert( left, word ), right)
      } else {
        FullNode( text, left, insert( right, word ))
      }
       
      return tree
    };
  }
}