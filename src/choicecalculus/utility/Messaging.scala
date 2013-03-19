package choicecalculus
package utility

/*
 * Extended the kiama messaging to support multiple types of messages 
 */

/*
 * This file is part of Kiama.
 *
 * Copyright (C) 2009-2013 Anthony M Sloane, Macquarie University.
 *
 * Kiama is free software: you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the
 * Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version.
 *
 * Kiama is distributed in the hope that it will be useful, but WITHOUT ANY
 * WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for
 * more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with Kiama.  (See files COPYING and COPYING.LESSER.)  If not, see
 * <http://www.gnu.org/licenses/>.
 */

/**
 * Facility for buffering of messages associated with positioned values.
 */
object Messaging {

    import scala.collection.mutable.{ListBuffer, StringBuilder}
    import scala.util.parsing.input.{Positional, Position}    
    import org.kiama.util.{Emitter, Positioned}    
    
    /**
     * A message record consisting of a coordinate position `pos` and
     * a `message` string.
     */
    abstract class Record {
      def pos: Position
      def message: String
      
      override def toString : String =
            pos.line + "." + pos.column + ": " + message
    }
    case class Info(pos : Position, message : String) extends Record
    case class Warning(pos : Position, message : String) extends Record
    case class Error(pos : Position, message : String) extends Record

    /**
     * Buffer of messages.
     */
    val messages = new ListBuffer[Record] ()

    /**
     * The messages sorted by increasing position.
     */
    def sortedmessages : Seq[Record] =
        messages.toList.sortWith (_.pos < _.pos)

    /**
     * Buffer a new message associated with the given `Positional` value.
     */
    def message (value : Positional, message : String) {
        messages += Info (value.pos, message)
    }

    /**
     * Buffer a new message associated with the given `Positioned` value.
     * The `finish` position is ignored at present.
     */
    def message (value : Positioned, message : String) {
        messages += Info (value.start, message)
    }

    /**
     * Return the number of messages that are buffered.
     */
    def messagecount : Int =
        messages.size

    /**
     * Output the messages in order of position using the given emitter, which
     * defaults to standard output.
     */
    def report (emitter : Emitter = new Emitter) {
        for (m <- sortedmessages)
            emitter.emitln (m)
    }

    /**
     * Reset the message buffer to empty.
     */
    def resetmessages () {
        messages.clear
    }

}