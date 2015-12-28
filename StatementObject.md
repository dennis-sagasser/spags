
```
namespace SPAGS
{
  enum StatementType
  {
    Block, If, While, Return, FunctionCall, MethodCall,
    Assignment,
    VariableDeclaration
  }

  abstract class Statement
  {
    StatementType Type;
  }

  class Statement.Block : Statement
  {
    List<Statement> ChildStatements;
    NameDictionary Scope;
  }

  class Statement.If : Statement
  {
    Expression IfThisIsTrue;
    Statement ThenDoThis;
    Statement ElseDoThis; // may be null
  }

  class Statement.Assignment : Statement
  {
    Expression Target; // Variable, Field, Attribute, ArrayIndex
    TokenType AssignmentType; // Assign, AddAssign, SubtractAssign, Increment, Decrement
    Expression Value; // may be null (Increment, Decrement)
  }

  class Statement.Return : Statement
  {
    Expression Value; // may be null
  }

  class Statement.Call : Statement
  {
    Expression Target;
  }

  class Statement.MethodCall : Statement
  {
  }

  class Statement.While : Statement
  {
    Expression WhileThisIsTrue;
    Statement KeepDoingThis;
  }
}
```