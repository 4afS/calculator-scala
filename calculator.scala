import scala.collection.mutable.Stack

sealed trait Token
sealed trait Term
case object Plus extends Token
case class Num(x: Int) extends Token with Term
case class Add(x: Term, y: Term) extends Token with Term

object calculator {
    def main(args: Array[String]) {
        val source = "123++"
        val tokens = lexer(source)
        val stack = parser(tokens)
        val ans = eval(stack)
        println("answer : " + ans)
    }

    def lexer(source :String):List[Token] = {
        var tokens = List[Token]()
        for(c <- source){
            c match {
                case ' ' =>
                case '+' => tokens :+= Plus
                case _ => tokens :+= Num(c.asDigit)
            }
        }
        tokens
    }

    def parser(tokens :List[Token]):Term = {
        val stack = new Stack[Term]
        for(token <- tokens){
            token match{
                case Num(x) => stack.push(Num(x))
                case _ => stack.push(Add(stack.pop, stack.pop))
            }
        }
        stack.pop
    }

    def eval(stacks :Term):Int = {
        stacks match{
            case Add(x, y) => eval(x) + eval(y)
            case Num(x) => x
        }
    }
}
