# include <iostream>
# include <cmath>
# include <sstream>
# include <stdio.h>
# include <string>
# include <string.h>
# include <vector>
# include <cstdlib>
# include <vector>
# include <cctype>
# include <iomanip>
# include <exception>
# include <stdexcept>
# include <list>
#include <math.h>
# include <stdlib.h>
using namespace std;

struct Node { // 節點 
  string name ;
  string type ;
  Node *parent ;
  Node *left ;
  Node *right ;
  vector<string> parameters ; // 需要的參數
  vector<Node * > function ; // 要執行的function
};

struct Token{ // 一個token
  string name  ; 
  int start  ;
  int end  ; 
  int line  ;
  string type ;
 
};

typedef Node * NodePtr ;

typedef struct Defined_Table{ // 存被定義過的東西 
  string name ;
  string type ;
  vector<string> parameter1 ; // 參數個數
  vector<string> parameter2 ; 
  vector<NodePtr> behavior ;
  Node * origin ; 
  Node * point ; 
  int from ; 
} Table ;

vector<Table> table ;

typedef struct LPinfo{
  bool quote ; // 前面有沒有引號 
  bool dot ; // 這顆區段裡有沒有逗點
  int LP_pos ; // 左括號的位置 
  
} LP ;

NodePtr ProcessFunc( NodePtr head, string & FuncName, int & error, vector<Table> local_table, string & FuncType ) ;

double Rounding( double num, int index ) {// 四捨五入用
  bool isNegative = false; // whether is negative number or not

  if ( num < 0 ) {// if this number is negative, then convert to positive number
    isNegative = true;
    num = -num;
  } // if

  if ( index >= 0 ) {
    int multiplier;
    multiplier = pow( 10, index );
    num = ( int ) ( num * multiplier + 0.5 ) / ( multiplier * 1.0 );
  } // if

  if ( isNegative ) num = -num ;
  
  return num;
} // Rounding() 

string TypeCheck( string str, Token & token ) { // 確認type 

  if ( str == "(" ) { // 左括號
    return "LP" ; 
  } // if
  else if ( str == ")" ) { // 左括號
    return "RP" ; 
  } // else if
  else if ( str == "." ) { // 左括號
    return "DOT" ; 
  } // else if	
  else if ( str == "nil" || str == "#f" ) { // 左括號
    token.name = "nil" ;
    return "NIL" ; 
  } // else if
  else if ( str == "#t" || str == "t"  ) {
    token.name = "#t" ;
    return "T" ;
  } // else if
  else if ( str == "\'"  ) {
    token.name = "quote" ;
    return "QUOTE" ;
  } // else if
  else if ( str == "()"  ) {
    return "LPRP" ;
  } // else if
  else if ( str[0] == '\"' ) {
    return "STRING" ;
  } // else if
  else {
  	bool isNum = true, isFloat = false ;
  	//cout << "----------------" << str.size() << endl;
  	for( int i = 0 ; i < str.length() ; i++ ){
  	  //cout << str[i] ;
	  if( i == 0 && ( str[i] == '+' || str[i] == '-' ) ) ;
      else if( str[i] >= '0' && str[i] <= '9' ) isNum = true ;
      else if( str[i] == '.' ) isNum = true, isFloat = true ;
      else isNum = false, isFloat = false, i = str.length()  ;	
	} // for
	
	//cout << "********************" << str << endl;
	if( ( str[0] == '+' || str[0] == '-' ) && str.length() == 1 ) ;
	else if( str[0] == '.' && str.length() == 1 ) ;
	else if( ( str[0] == '+' || str[0] == '-' ) && str[1] == '.' && str.length() == 2 ) ;
	else{
	  token.name = str ;
	  if( isNum ){
	    if( str[0] == '+' ){
	      if( str.size() > 1 && str[1] == '+' ) ; //++
		  else str.erase( str.begin() ), token.name = str ;
		} // if
	  	
		if( isFloat ){
		  double temp = atof( str.c_str() ) ;
		  temp = Rounding( temp, 3 ) ;
          char ans[20] ;
          sprintf( ans, "%.*f", 3, temp ) ;
          str = ans ;
          if( str[0] == '.' ) str = "0" + str ;
          //cout << "FLOAT-------------" << str << endl;
          token.name = str ;
          return "FLOAT" ;
		} // if
		else return "INT" ;
	  } // if
	  
	} // else
    return "SYMBOL" ;
  } // else 

} // TypeCheck()

bool Atom( Token token ) { // 判斷是不是ATOM
  if ( token.type == "SYMBOL" ) 
    return true ;
  else if ( token.type == "INT" )
    return true ;
  else if ( token.type == "FLOAT" )
    return true ;
  else if ( token.type == "STRING" )
    return true ;
  else if ( token.type == "NIL" )
    return true ;
  else if ( token.type == "T" )
    return true ;
  else if ( token.type == "LPRP" )
    return true ; 
  else
    return false ;
} // Atom()

bool Parameter( char Char, bool & quote, bool & comment, bool & ChangeLine, int & error ) { // parameter
   
  if ( Char == ' ' || Char == '(' || Char == ')' || Char == '\'' || Char == '\"' || Char == ';'
        || Char == '\n' ) {
    if ( !quote && Char == '\"' ) quote = true ;  
    else if ( quote && Char == '\"' ) quote = false ; 
    else if ( quote && Char == '\n' ) {
      // 字串到一辦換行
      error = 3 ;
      return false ;
    } // else if
    else if ( quote && Char == ';' ) return true ; 
    else { // 沒有註解的情況oint下遇到括號 
      if ( Char == ';' ) comment = true ;  
      else if ( Char == '\n' ) ChangeLine = true ;
        
    } // else

    return true ; 
  } // if
  else
    return false ;
} // Parameter()

bool IsSep( char alpha, bool & QTE, int & error, bool & comment, bool & changeL ) { // 是不是seperator
  // QTE->引號, R-括號數量, error->錯誤代碼, comment->註解, RP->右括號 
   
  if ( alpha == ' ' || alpha == '(' || alpha == ')' || alpha == '\'' ||
       alpha == '\"' || alpha == ';' || alpha == '\n' ) {
    if ( QTE == false && alpha == '\"' ) {
      QTE = true ;
    } // if
    else if ( QTE == true && alpha == '\"' ) {
      QTE = false ;
    } // else if
    else if ( QTE == true && alpha == '\n' ) {
      // 讀字串時遇到的2種狀況
      // 1-換行(eof在外面解決), 2-;註解
      
      error = 3 ;
      return false ;
    } // else if
    else if ( QTE == true && alpha == ';' ) {
      return true ;
    } // else if
    else { // 沒有註解的情況下遇到括號 
      if ( alpha == ';' ) {
        comment = true ;
      } // else if
      else if ( alpha == '\n' ) {
        changeL = true ;
      } // else if 
    } // else

    return true ; 
  } // if
  else {
    return false ;
  } // else
} // IsSep()

Token GetToken( int & err, bool & eof, int & line, int & column, string & next, bool & changeL,
                vector<Token> a, bool & Fline ) {
  // next->下一個字的開頭
  // changeL->是否換行了(主要用來處理錯誤後讀行), Fline->判斷該行是否要加一 ex: 3\n  
  char alpha = '\0' ;
  string setn = "\0" ;
  int q = 0 ; // 引號個數 
  bool fspace = true, comment = false, qte = false ; // Fspace->前空白, comment->註解, QTE->引號, R->右括號在字後面 
  Token tmp ;


  if ( next != "\0" && err == 0 ) { // 上次讀到的sep
    //cout << "0********************" << next  << endl;
    if ( next == "(" ) {
    	//cout << "0.1********************" << endl;
      tmp.start = column ;
      fspace = false ;
      setn = next ;
      column = column + 1 ;
      next = "\0" ;
    } // if
    else if ( next == "\"" ) {
    	//cout << "0.2********************" << endl;
      fspace = false ;
      tmp.start = column ;
      qte = true ;
      setn = next ;
      q = q + 1 ;
      column = column + 1 ;
      next = "\0" ;
    } // else if
    else if ( next == ";" ) {
    	//cout << "0.3********************" << endl;
      fspace = false ;
      comment = true ;
      column = column + 1 ;
      next = "\0" ;
    } // else if
    else if ( next == " " || next == "\n" ) {
    	//cout << "0.4********************" << endl;
      if ( next == " " ) {
      	//cout << "0.42********************" << endl;
        column = column + 1 ;
        next = "\0" ;
      } // if
      else { // 3\n行不變 
      //cout << "0.44********************" << endl;
        if ( Fline == true ) {
          line = 1 ;
        } // if
        else
          line = line + 1 ;

        column = 1 ;
        Fline = false ;
        changeL = false ; // 要把上一個讀到換行的後果解決 
        next = "\0" ; 
      } // else

    } // else if 
    else {
    	///cout << "0.5********************" << endl;
      setn = next ;
      next = "\0" ;

      if ( setn == "'" || setn == ")"  ) {
        tmp.start = column ;
        tmp.end = tmp.start  ;
        tmp.line = line ;
        tmp.name = setn ;
        tmp.type = TypeCheck( setn, tmp ) ;
        column = column + 1 ;
        return tmp ;
      } // if
      else {
        tmp.start = column ;
        column = column + 1 ;
        fspace = false ;
      } // else
    } // else

  } // if
  else if ( err != 0 ) {
    fspace = false ;
  } // else if

  while ( cin.get( alpha ) ) { 
    //cout << setn << " " << alpha << endl;
    if ( alpha == '' ) { // eof
    //cout << "1*****************" << endl;
      eof = true ;
      if ( qte == true ) { // 在有引號的情況下遇到eof
        err = 3 ;
        return tmp ; 
      } // if
      else if ( comment == true ) { // 遇到eof前面有註解
        return tmp ; 
      } // else if 
      else if ( setn != "\0" ) { // 遇到eof前有獨到字串
        err = 0 ;
        tmp.end = column - 1 ;
        tmp.line = line ;
        tmp.name = setn ;
        tmp.type = TypeCheck( setn, tmp ) ;

        return tmp ;
      } // else if
      else { // 直接EOF
        err = 4 ; 
        return tmp ;
      } // else if 
    } // if
    else if ( ( alpha == ' ' || alpha == '\n' ) && fspace == true  ) { // 前空白尚未讀完
    //cout << "2*****************" << endl;
      if ( alpha == '\n'  ) { // 遇到換行 	
        if ( Fline == true  ) { // 換新的，但是那行只有前一個用的token ex->3\n
          line = 1 ;
          column = 0 ;
          Fline = false ; 
        }  // if 
        else {
          line = line + 1 ;
          column = 0 ;
        } // else 

      } // if
      
    } // else if
    else if ( err != 0 ) { // 遇到錯誤將後面讀完
    //cout << "3*****************" << endl;
      if ( alpha == '\n' ) { // 遇到換行 
        line = 1 ;
        column = 0 ;
        err = 0 ;
        Fline = false ;
        fspace = true ;
      } // if  
    } // else if 
    else if ( comment == true  ) { // 遇到註解要讀到換行符號 
    //cout << "4*****************" << endl;
      if ( alpha == '\n' ) { // 遇到換行
        if ( Fline == true  ) { // 換新的，但是那行只有前一個用的token ex->3\n 
          line = 1 ;
          column = 0 ;
          Fline = false ;
        }  // if 
        else        
          line = line + 1 ;

        column = 0 ;
        err = 0 ;
        comment = false ;
        fspace = true ;
      } // if
    } // else if
    else {
    //cout << "5*****************" << endl;
      if ( alpha != ';' )
        Fline = false ;
      
      if ( IsSep( alpha, qte, err, comment, changeL ) == true && fspace == true ) { // 第一個直接遇到seperator
      //cout << "5.2*****************" << endl;
        fspace = false ;
        if ( alpha == ')' || alpha == '\'' ) { // 這兩種直接回傳 
          setn = alpha ;
          tmp.start = column ;
          tmp.end = column ;
          tmp.line = line ;
          tmp.name = setn ;
          tmp.type = TypeCheck( setn, tmp ) ;
          column = column + 1 ;
          return tmp ;
        } // if
        else if ( alpha == '\"' || alpha == '(' ) {
          if ( alpha == '\"' ) {
            q = q + 1 ;
          } // if

          setn = alpha ;
          tmp.start = column ;
        } // else if 
      } // if 
      else { // 要結束這次讀字串了
      //cout << "5.4*****************" << endl;
        fspace = false ;
        if ( IsSep( alpha, qte, err, comment, changeL ) == true ) { // 遇到seperator
		//cout << "5.42*****************" << endl; 

          if ( setn == "(" && ( alpha == ' ' || alpha == '\n' ) ) {
            if ( alpha == '\n' ) {
              line = line + 1 ;
              column = 0 ;
            } // if
          } // if
          else if ( setn == "(" && alpha == ')' ) { // ()->nil
            setn = "nil" ;
            tmp.end = tmp.start + 1  ;
            tmp.line = line ;
            tmp.name = "nil" ;
            tmp.type = TypeCheck( setn, tmp ) ;
            column = column + 1 ;
            return tmp ;
          } // else if
          else if ( setn == "(" && alpha != ')' ) { // ( + 其他sep 
            tmp.end = tmp.start  ;
            tmp.line = line ;
            tmp.name = "(" ;
            tmp.type = TypeCheck( setn, tmp ) ;

            // 不是這兩者而是接下來會用到 
            next = alpha ;
            return tmp ;
          } // else if 
          else if ( alpha == '\"' && q == 1 && setn[setn.size() - 1] != '\\' ) { // string 結束
            // setn[setn.size() - 1] != '\\'-> 防止 \"的出現 
            setn = setn + alpha ; // "1 + " = "1"
            
            tmp.end = column ;
            tmp.line = line ;
            tmp.name = setn ;
            tmp.type = TypeCheck( setn, tmp ) ;
            column = column + 1 ;

            return tmp ;
          } // else if
          else if ( qte == true ) {
            if ( alpha == '\n' ) { // 引號中遇到換行
              err = 3 ;
              return tmp ; 
            } // if
            else { //  
              setn = setn + alpha ;
            } // else  
          } // else if
          else { // 找到並回傳 
            next = alpha ;
            tmp.end = column - 1  ;
            tmp.line = line ;
            tmp.name = setn ;
            tmp.type = TypeCheck( setn, tmp ) ;

            return tmp ;
          } // else
        } // if
        else if ( setn == "(" && alpha != ')' ) { // ( + 非sep
		//cout << "5.44*****************" << endl; 
          tmp.end = tmp.start ;
          tmp.line = line ;
          tmp.name = "(" ;
          tmp.type = TypeCheck( setn, tmp ) ;

          next = alpha ;
          return tmp ;
        } // else if 
        else if ( qte == true ) { // 引號中 
        //cout << "5.46*****************" << endl;
             
          if ( alpha == '\n' ) { // 引號中遇到換行
            err = 3 ;
            return tmp ; 
          } // if
          else { //  
            setn = setn + alpha ;
          } // else    	
        } // else if
        else {
          //cout << "5.48********************** " << alpha << " " << column << endl;
          if ( setn == "\0" ) { // 第一個字元  
            setn = alpha ;
            tmp.start = column ;
          } // if
          else {
            setn = setn + alpha ;
            //cout << "---------------------" << setn << endl;
          } // else
        } // else 
      } // else
    } // else 

    column = column + 1 ;
  } // while

 
  eof = true ;
  if ( qte == true ) { // 在有引號的情況下遇到eof
    err = 3 ;
    return tmp ; 
  } // if
  else if ( comment == true ) { // 遇到eof前面有註解
    return tmp ; 
  } // else if 
  else if ( setn != "\0" ) { // 遇到eof前有獨到字串
    tmp.end = column - 1 ;
    tmp.line = line ;
    tmp.name = setn ;
    tmp.type = TypeCheck( setn, tmp ) ;

    return tmp ;
  } // else if
  else { // 直接eof
    err = 4 ; 
  } // else

  return tmp ;
} // GetToken() 

bool IsSExp( vector<Token> & a, int & err, bool & eof, int & line, int & column,  string & nextSep,
             bool & changeL, bool & Fline ) {
  // list->一個sexp,  err->錯誤代碼, eof->eof是否出現, last->上一個token, line->行數, column->字元數 
  // nextSep->下一個seperator的開頭 
  // 判斷是否是SExp並且確認有無錯誤的發生
  Token next ;
  
  next = GetToken( err, eof, line, column, nextSep, changeL, a, Fline ) ;

  if ( eof == true && err == 4 ) {
    // 讀token時遇到eof

    return false ;
  } // if
  else if ( err == 0 && Atom( next ) == true  ) {
    // 只有atom   	

    a.push_back( next ) ;
    return true ;
  } // else if
  else if ( err == 0 && next.type == "LP" ) {
    // 開頭是LP 

    a.push_back( next ) ;
    if ( IsSExp( a, err, eof, line, column, nextSep, changeL, Fline ) == true ) {

      while ( eof != true && IsSExp( a, err, eof, line, column, nextSep, changeL, Fline ) == true ) {
        ;
      } // while

      if ( eof == true ) {
        err = 2 ; 
        return false ;
      } // if
      else {
        if ( a[a.size()-1].type == "DOT" && a[a.size()-2].name != "("  && err != 2  ) {
          err = 0 ;
          if ( IsSExp( a, err, eof, line, column, nextSep, changeL, Fline ) == true ) {
            next = GetToken( err, eof, line, column, nextSep, changeL, a, Fline ) ;
            a.push_back( next ) ;

            if ( next.type == "RP" ) {
              err = 0 ;
              return true ;
            } // if
            else {
              err = 2 ;
              return false ;
            } // else
          } // if
          else {
            return false ;
          } // else
        } // if
        else if ( err == 2 ) {
          return 2 ;
        } // else if 
        else if ( a[a.size()-1].type == "RP" ) {
          err = 0 ;
          return true ;
        } // else if
        else if ( err == 3 ) {
          return false ;
        } // else if
        else {
          err = 1 ;
          return false ;
        }  // else
      } // else
    } // if
    else {
      return false ;
    } // else
  } // else if
  else if ( err == 0 && next.type == "QUOTE" ) {
    // 開頭是 QUOTE 
    a.push_back( next ) ;
    if ( IsSExp( a, err, eof, line, column, nextSep, changeL, Fline ) == true ) {

      return true ;
    } // if
    else
      return false ;
  } // else if
  else {

    if ( err == 0 && next.name != "\0"  ) {
      err = 1 ;
    } // if
    else if ( err == 3 ) {
      err = 3 ;
    } // else if 
    else {
      err = 4 ;
    } // else 

    a.push_back( next ) ;
    return false ;
  } // else
} // IsSExp()

void ReadSExp( vector<Token> & a, int & err, bool & eof, int & line, int & column, string & nextSep, bool & changeL, bool & Fline  ) {              
  // 透過 IsSEep拿到一個s-expression
  IsSExp( a, err, eof, line, column, nextSep, changeL, Fline ) ;// 是否符合文法
  /*
  if ( err == 0 ) {
    for ( int i = 0; i <= list.size() - 1 ; i++ )
      cout << list[i].name << " " << list[i].line << " " << list[i].start << " " << list[i].end << "\n" ;
  } // if
  */ 

  
} // ReadSExp()
/* 
Token GetToken( vector<Token> a, int & line, int & column, int & err, bool & eof,  string & nextToken, bool & changeL, bool & Fline ) {
  // nextToken->下一個字的開頭
  // changeL->是否換行了(主要用來處理錯誤後讀行), Fline->判斷該行是否要加一 ex: 3\n  
  char alpha = '\0' ;
  string setn = "\0" ;
  int q = 0 ; // 引號個數 
  bool space = true, comment = false, qte = false ; // space->前空白, comment->註解, QTE->引號, R->右括號在字後面 
  Token temp ;


  if ( nextToken != "\0" && err == 0 ) { // 上次讀到的sep
    if ( nextToken == "(" ) {
      temp.start = column ;
      space = false ;
      setn = nextToken ;
      column = column + 1 ;
      nextToken = "\0" ;
    } // if
    else if ( nextToken == "\"" ) {
      space = false ;
      temp.start = column ;
      qte = true ;
      setn = nextToken ;
      q = q + 1 ;
      column = column + 1 ;
      nextToken = "\0" ;
    } // else if
    else if ( nextToken == ";" ) {
      space = false ;
      comment = true ;
      column = column + 1 ;
      nextToken = "\0" ;
    } // else if
    else if ( nextToken == " " || nextToken == "\n" ) {
      if ( nextToken == " " ) {
        column = column + 1 ;
        nextToken = "\0" ;
      } // if
      else { // 3\n行不變 
        if ( Fline == true ) {
          line = 1 ;
        } // if
        else
          line = line + 1 ;

        column = 1 ;
        Fline = false ;
        changeL = false ; // 要把上一個讀到換行的後果解決 
        nextToken = "\0" ; 
      } // else

    } // else if 
    else {
      setn = nextToken ;
      nextToken = "\0" ;

      if ( setn == "'" || setn == ")"  ) {
        temp.start = column ;
        temp.end = temp.start  ;
        temp.line = line ;
        temp.name = setn ;
        temp.type = TypeCheck( setn, temp ) ;
        column = column + 1 ;
        return temp ;
      } // if
      else {
        temp.start = column ;
        column = column + 1 ;
        space = false ;
      } // else
    } // else

  } // if
  else if ( err != 0 ) {
    space = false ;
  } // else if

  while ( cin.get( alpha ) ) { 

    if ( alpha == '' ) { // eof
      eof = true ;
      if ( qte == true ) { // 在有引號的情況oint下遇到eof
        err = 3 ;
        return temp ; 
      } // if
      else if ( comment == true ) { // 遇到eof前面有註解
        return temp ; 
      } // else if 
      else if ( setn != "\0" ) { // 遇到eof前有獨到字串
        err = 0 ;
        temp.end = column - 1 ;
        temp.line = line ;
        temp.name = setn ;
        temp.type = TypeCheck( setn, temp ) ;

        return temp ;
      } // else if
      else { // 直接EOF
        err = 4 ; 
        return temp ;
      } // else if 
    } // if
    else if ( ( alpha == ' ' || alpha == '\n' ) && space == true  ) { // 前空白尚未讀完
      if ( alpha == '\n'  ) { // 遇到換行 	
        if ( Fline == true  ) { // 換新的，但是那行只有前一個用的token ex->3\n
          line = 1 ;
          column = 0 ;
          Fline = false ; 
        }  // if 
        else {
          line = line + 1 ;
          column = 0 ;
        } // else 

      } // if
      
    } // else if
    else if ( err != 0 ) { // 遇到錯誤將後面讀完
      if ( alpha == '\n' ) { // 遇到換行 
        line = 1 ;
        column = 0 ;
        err = 0 ;
        Fline = false ;
        space = true ;
      } // if  
    } // else if 
    else if ( comment == true  ) { // 遇到註解要讀到換行符號 
      if ( alpha == '\n' ) { // 遇到換行
        if ( Fline == true  ) { // 換新的，但是那行只有前一個用的token ex->3\n 
          line = 1 ;
          column = 0 ;
          Fline = false ;
        }  // if 
        else        
          line = line + 1 ;

        column = 0 ;
        err = 0 ;
        comment = false ;
        space = true ;
      } // if
    } // else if
    else {
      if ( alpha != ';' )
        Fline = false ;
      
      if ( Parameter( alpha, qte, comment, changeL, err  ) == true && space == true ) { // 第一個直接遇到seperator
        space = false ;
        if ( alpha == ')' || alpha == '\'' ) { // 這兩種直接回傳 
          setn = alpha ;
          temp.start = column ;
          temp.end = column ;
          temp.line = line ;
          temp.name = setn ;
          temp.type = TypeCheck( setn, temp ) ;
          column = column + 1 ;
          return temp ;
        } // if
        else if ( alpha == '\"' || alpha == '(' ) {
          if ( alpha == '\"' ) {
            q = q + 1 ;
          } // if

          setn = alpha ;
          temp.start = column ;
        } // else if 
      } // if 
      else { // 要結束這次讀字串了
        space = false ;
        if ( Parameter( alpha, qte, comment, changeL, err ) == true ) { // 遇到seperator 

          if ( setn == "(" && ( alpha == ' ' || alpha == '\n' ) ) {
            if ( alpha == '\n' ) {
              line = line + 1 ;
              column = 0 ;
            } // if
          } // if
          else if ( setn == "(" && alpha == ')' ) { // ()->nil
            setn = "nil" ;
            temp.end = temp.start + 1  ;
            temp.line = line ;
            temp.name = "nil" ;
            temp.type = TypeCheck( setn, temp ) ;
            column = column + 1 ;
            return temp ;
          } // else if
          else if ( setn == "(" && alpha != ')' ) { // ( + 其他sep 
            temp.end = temp.start  ;
            temp.line = line ;
            temp.name = "(" ;
            temp.type = TypeCheck( setn, temp ) ;

            // 不是這兩者而是接下來會用到 
            nextToken = alpha ;
            return temp ;
          } // else if 
          else if ( alpha == '\"' && q == 1 && setn[setn.size() - 1] != '\\' ) { // string 結束
            // setn[setn.size() - 1] != '\\'-> 防止 \"的出現 
            setn = setn + alpha ; // "1 + " = "1"
            
            temp.end = column ;
            temp.line = line ;
            temp.name = setn ;
            temp.type = TypeCheck( setn, temp ) ;
            column = column + 1 ;

            return temp ;
          } // else if
          else if ( qte == true ) {
            if ( alpha == '\n' ) { // 引號中遇到換行
              err = 3 ;
              return temp ; 
            } // if
            else { //  
              setn = setn + alpha ;
            } // else  
          } // else if
          else { // 找到並回傳 
            nextToken = alpha ;
            temp.end = column - 1  ;
            temp.line = line ;
            temp.name = setn ;
            temp.type = TypeCheck( setn, temp ) ;

            return temp ;
          } // else
        } // if
        else if ( setn == "(" && alpha != ')' ) { // ( + 非sep 
          temp.end = temp.start ;
          temp.line = line ;
          temp.name = "(" ;
          temp.type = TypeCheck( setn, temp ) ;

          nextToken = alpha ;
          return temp ;
        } // else if 
        else if ( qte == true ) { // 引號中 
             
          if ( alpha == '\n' ) { // 引號中遇到換行
            err = 3 ;
            return temp ; 
          } // if
          else { //  
            setn = setn + alpha ;
          } // else    	
        } // else if
        else {
          if ( setn == "\0" ) { // 第一個字元 
            setn = alpha ;
            temp.start = column ;
          } // if
          else {
            setn = setn + alpha ;
          } // else
        } // else 
      } // else
    } // else 

    column = column + 1 ;
  } // while

 
  eof = true ;
  if ( qte == true ) { // 在有引號的情況oint下遇到eof
    err = 3 ;
    return temp ; 
  } // if
  else if ( comment == true ) { // 遇到eof前面有註解
    return temp ; 
  } // else if 
  else if ( setn != "\0" ) { // 遇到eof前有獨到字串
    temp.end = column - 1 ;
    temp.line = line ;
    temp.name = setn ;
    temp.type = TypeCheck( setn, temp ) ;

    return temp ;
  } // else if
  else { // 直接eof
    err = 4 ; 
  } // else

  return temp ;
} // GetToken() 
*/ 
/* 
bool Sexp( vector<Token> & a, int & err, bool & eof, int & line, int & column,  string & nextToken, bool & ChangeLine, bool & Newline ) {            
  // list->一個sexp,  err->錯誤代碼, eof->eof是否出現, last->上一個token, line->行數, column->字元數 
  // nextToken->下一個seperator的開頭 
  // 判斷是否是SExp並且確認有無錯誤的發生
  Token token ;
  
  token = GetToken( a, line, column, err, eof, nextToken, ChangeLine,  Newline ) ;
  cout << "0!!!!!!!!!!!!!!!!!!!!!!" << token.name << endl;
  if ( eof == true && err == 4 ) {
    // 讀tokn時遇到eof
    cout << "1!!!!!!!!!!!!!!!!!!!!!!" << endl;
    return false ;
  } // if
  else if ( err == 0 && Atom( token ) == true  ) {
    // 只有atom   	
    cout << "2!!!!!!!!!!!!!!!!!!!!!!" << endl;
    a.push_back( token ) ;
    return true ;
  } // else if
  else if ( err == 0 && token.type == "LP" ) {
    // 開頭是LP 
	cout << "3!!!!!!!!!!!!!!!!!!!!!!" << endl;
    a.push_back( token ) ;
    if ( Sexp( a, err, eof, line, column, nextToken, ChangeLine, Newline ) == true ) {
      cout << "3.2!!!!!!!!!!!!!!!!!!!" << endl;
      while ( eof != true && Sexp( a, err, eof, line, column, nextToken, ChangeLine, Newline ) == true ) {
        ;
      } // while

      if ( eof == true ) {
        err = 2 ; 
        return false ;
      } // if
      else {
      	cout << "3.4!!!!!!!!!!!!!!!!!!!" << endl;
        if ( a[a.size()-1].type == "DOT" && ( a.size() - 2 > 0 && a[a.size()-2].name != "(" )  && err != 2  ) {
          err = 0 ;
          if ( Sexp( a, err, eof, line, column, nextToken, ChangeLine, Newline ) == true ) {
            token = GetToken( a, line, column, err, eof, nextToken, ChangeLine,  Newline ) ;
            a.push_back( token ) ;

            if ( token.type == "RP" ) {
              err = 0 ;
              return true ;
            } // if
            else {
              err = 2 ;
              return false ;
            } // else
          } // if
          else {
            return false ;
          } // else
        } // if
        else if ( err == 2 ) {
          return 2 ;
        } // else if 
        else if ( a[a.size()-1].type == "RP" ) {
          err = 0 ;
          return true ;
        } // else if
        else if ( err == 3 ) {
          return false ;
        } // else if
        else {
          err = 1 ;
          return false ;
        }  // else
      } // else
    } // if
    else {
      return false ;
    } // else
  } // else if
  else if ( err == 0 && token.type == "QUOTE" ) {
    // 開頭是 QUOTE 
    cout << "4!!!!!!!!!!!!!!!!!!!!!!" << endl;
    a.push_back( token ) ;
    if ( Sexp( a, err, eof, line, column, nextToken, ChangeLine, Newline ) == true ) {

      return true ;
    } // if
    else
      return false ;
  } // else if
  else {
    cout << "5!!!!!!!!!!!!!!!!!!!!!!" << endl;
    if ( err == 0 && token.name != "\0"  ) {
      err = 1 ;
    } // if
    else if ( err == 3 ) {
      err = 3 ;
    } // else if 
    else {
      error = 4 ;
    } // else 

    a.push_back( token ) ;
    return false ;
  } // else
} // Sexp()
*/ 

void BuildTree( vector<Token> a, NodePtr & head, Node * parent,
                int & count, bool & dot, vector<LP> & LPB ) {
  // 將拿到的token串拿來建樹 
  // BuildTree( a, head, parent, count, dot ) ;
  NodePtr now = NULL ; // 接下來要插入的地點

  if ( head != NULL ) { // 用來找放置地點

    while ( parent != head && parent->left != NULL && parent->right != NULL ) {
      parent = parent->parent ; 
    } // while

    if ( parent != NULL ) {
      if ( parent->left == NULL ) {
        now = parent->left ;
      } // if
      else {
        now = parent->right ;
      } // else

    }  // if
  } // if
 
  
  if ( count == a.size()  ) {
    ;
  } // if
  else if ( head == NULL ) { // 一開頭
    head = new Node ;
    head->parent = NULL ;
    head->left = NULL ;
    head->right = NULL ;
    head->name = "\0" ;

    
    if ( a[count].name == "(" ) {
      LP tmp ;
      tmp.dot = false ;
      tmp.LP_pos = count ;
      tmp.quote = false ;
      LPB.push_back( tmp ) ;

      count = count + 1 ;
      BuildTree( a, head, head, count, dot, LPB ) ;
    } // if
    else {
      BuildTree( a, head, head, count, dot, LPB ) ;
    } // else
  } // else if
  else if ( a[count].name == "." ) {
    dot = true ;
    count = count + 1 ; 
    LPB[LPB.size() - 1].dot = true ;

    BuildTree( a, head, parent, count, dot, LPB ) ;
  } // else if
  else if ( a[count].name == "(" ) {

    LP tmp ;
    tmp.dot = false ;
    tmp.LP_pos = count ;
    if ( a[count - 1].name == "quote" )
      tmp.quote = true ;
    else 
      tmp.quote = false ;
      
    LPB.push_back( tmp ) ;
   
    if ( now == parent->left ) {
      //cout << "----------------" << endl;
      now = new Node ;
      parent->left = now ;
    } // if 
    else {
      now = new Node ;
      parent->right = now ;
    } // else

    now->parent = parent ;
    now->left = NULL ;
    now->right = NULL ;
    now->name = "\0" ;
  
    if ( dot == true || now == parent->left ) {
      dot = false ;
      parent = now ;
    } // if
    else if ( now == parent->right ) {
      parent = now ;
      parent->left = new Node ;
    
      now = parent->left ;
      now->left = NULL ;
      now->right = NULL ;
      now->name = "\0" ;
      now->parent = parent ;
      parent = now ;
    } // else if  

    count = count + 1 ;
    BuildTree( a, head, parent, count, dot, LPB ) ;
  } // else if 
  else if ( dot == true && a[count].name != ")" && a[count].type != "QUOTE" ) {
    now = new Node ;
    parent->right = now ;
    now->parent = parent ;
    now->left = NULL ;
    now->right = NULL ;
    now->name = a[count].name ;
    now->type = a[count].type ;
    

    count = count + 1 ;
    BuildTree( a, head, parent, count, dot, LPB ) ;
  } // else if
  else if ( a[count].name == ")" ) {

    bool next = false ; // 是否執行下個動作   
    if ( LPB[LPB.size() - 1].quote == true ) { // 這個區段開頭有括號 
      if ( LPB[LPB.size() - 1].dot != true  ) {
        next = true ;
      } // if
      
      LPB.erase( LPB.end() - 1 ) ;
    } // if
    else { 
      if ( LPB[LPB.size() - 1].dot != true )
        next = true ;
        
      LPB.erase( LPB.end() - 1 ) ;
    } // else 
 
    if ( now == NULL && next == true  ) { 
      now = new Node ;
      parent->right = now ;      
      now->parent = parent ;
      now->left = NULL ;
      now->right = NULL ;
      now->name = "nil" ;
      now->type = "NIL" ;
    } // else if

   
    dot = false ;
    count = count + 1 ;
    BuildTree( a, head, parent, count, dot, LPB ) ;

  } // else if
  else {
    if ( a[count].type == "QUOTE" ) { 
      if ( count != 0 && ( dot == true || a[count - 1].type == "QUOTE" ) ) {
        if ( now == parent->left ) {
          now = new Node ;
          parent->left = now ;
        } // if 
        else {
          now = new Node ;
          parent->right = now ;
        } // else

        now->left = NULL ;
        now->right = NULL ;
        now->parent = parent ;
        now->name = "\0" ;
        parent = now ;
        now = parent->left ;      
      } // if
      else if ( count != 0 && dot == false && a[count - 1].type != "QUOTE" ) {
        if ( now == parent->left ) {
          now = new Node ;
          parent->left = now ;
          now->left = NULL ;
          now->right = NULL ;
          now->parent = parent ;
          now->name = "\0" ;
          parent = now ;
          now = parent->left ; 
        } // if 
        else {
          now = new Node ;
          parent->right = now ;
          now->left = NULL ;
          now->right = NULL ;
          now->parent = parent ;
          now->name = "\0" ;
          parent = now ;

          parent->left = new Node ;          
          now = parent->left ;
          now->left = NULL ;
          now->right = NULL ;
          now->parent = parent ;
          now->name = "\0" ;
          parent = now ;         
        } // else     
      } // else if

      now = new Node ;    
      parent->left = now ; 
      now->parent = parent ;
      now->left = NULL ;
      now->right = NULL ;
      now->name = a[count].name ;
      now->type = a[count].type ;

      parent->right = new Node ;
      now = parent->right ;
      now->parent = parent ;
      now->left = NULL ;
      now->right = new Node ;
      now->name = "\0" ;
      parent = now ;  

      now = parent->right ;
      now->parent = parent ;
      now->left = NULL ;
      now->right = NULL ;
      now->name = "nil" ;
      now->type = "NIL" ;

      dot = false ;
      count = count + 1 ;
      BuildTree( a, head, parent, count, dot, LPB ) ;
    } // if
    else { // 單純放值
      if ( now == parent->left || dot == true ) {
        now = new Node ;
        if ( dot == false )
          parent->left = now ;
        else 
          parent->right = now ;
 
        now->parent = parent ;
        now->left = NULL ;
        now->right = NULL ;
        now->name = a[count].name ;
        now->type = a[count].type ;
      } // if
      else {
        now = new Node ;
        parent->right = now ;
        now->parent = parent ;
        now->left = new Node ;
        now->right = NULL ;
        now->name = "\0" ;
        parent = now ;

        now = now->left ;
        now->parent = parent ;
        now->left = NULL ;
        now->right = NULL ;
        now->name = a[count].name ;
        now->type = a[count].type ;

      } // else if

      count = count + 1 ;
      BuildTree( a, head, parent, count, dot, LPB ) ;
    } // else 
  } // else 
} // BuildTree()

/* 
void BuildTree( vector<Token> workspace, NodePtr & head, Node * ParenNode, int & count, bool & dot, vector<LP> & LPinfo ) {
  NodePtr pos = NULL ;
  if ( head != NULL ) { // 用來找放置安orkspace點

    while ( ParenNode->left != NULL && ParenNode->right != NULL && ParenNode != head ) {
      //當現在這個節點有東西時(代表這個節點是有存東西的，相當於1個括號結束時往回走繼續建樹)，往回找節點
      ParenNode = ParenNode->parent ; 
    } // while

    if ( ParenNode != NULL ) {
      if ( ParenNode->left == NULL ) pos = ParenNode->left ;
      else pos = ParenNode->right ;

    }  // if
  } // if
 
  if ( count == workspace.size()  ) ;
  else if ( head == NULL ) { // 開頭
    head = new Node ;
    head->parent = NULL ;
    head->left = NULL ;
    head->right = NULL ;
    head->name = "\0" ;

    if ( workspace[count].name == "(" ) {
      LP tmp ;
      tmp.dot = false ;
      tmp.LP_pos = count ;
      tmp.quote = false ;
      LPinfo.push_back( tmp ) ;

      count = count + 1 ;
      BuildTree( workspace, head, head, count, dot, LPinfo ) ;
    } // if
    else {
      BuildTree( workspace, head, head, count, dot, LPinfo ) ;
    } // else
  } // else if
  else if ( workspace[count].name == "." ) {
    dot = true ;
    count = count + 1 ; 
    LPinfo[LPinfo.size() - 1].dot = true ;

    BuildTree( workspace, head, ParenNode, count, dot, LPinfo ) ;
  } // else if
  else if ( workspace[count].name == "(" ) {
    LP temp ;
    temp.dot = false ;
    temp.LP_pos = count ;
    if ( workspace[count - 1].name == "quote" ) temp.quote = true ;   
    else temp.quote = false ;
        
    LPinfo.push_back( temp ) ;
    pos = new Node ;//在那個位置新增節點 
    //把parent的子節點指向pos
    //cout << "+++++++++++++++++++" << endl;
    if ( pos == ParenNode->left ) cout << "1++++++++++++++++++" << endl, ParenNode->left = pos ;
    else if( pos == ParenNode->right ) cout << "2+++++++++++++++" << endl ;
    else if( pos == NULL ) cout << "3+++++++++++++++++" << endl, ParenNode->right = pos ;

    pos->parent = ParenNode ;
    pos->left = NULL ;
    pos->right = NULL ;
    pos->name = "\0" ;
    
    if ( pos == ParenNode->left || dot ) { //往左邊移 
      dot = false ;
      ParenNode = pos ;
    } // if
    else if ( pos == ParenNode->right ) { //如果現在的位置是在右邊，就往右邊移之後，再往左新增一個節點(因為資料都放左節點) 
      ParenNode = pos ;
      ParenNode->left = new Node ;
    
      pos = ParenNode->left ;
      pos->left = NULL ;
      pos->right = NULL ;
      pos->name = "\0" ;
      pos->parent = ParenNode ;
      ParenNode = pos ;
    } // else if  

    count = count + 1 ;
    BuildTree( workspace, head, ParenNode, count, dot, LPinfo ) ;
  } // else if 
  else if ( workspace[count].name != ")" && workspace[count].type != "QUOTE" && dot ) { //是dot的話放右邊 
    pos = new Node ;
    ParenNode->right = pos ;
    pos->parent = ParenNode ;
    pos->left = NULL ;
    pos->right = NULL ;
    pos->name = workspace[count].name ;
    pos->type = workspace[count].type ;
    
    count = count + 1 ;
    BuildTree( workspace, head, ParenNode, count, dot, LPinfo ) ;
  } // else if
  else if ( workspace[count].name == ")" ) {

    bool KeepGo = false ; // 是否執行下個動作 
    if ( LPinfo[LPinfo.size() - 1].quote  ) {
      if ( !LPinfo[LPinfo.size() - 1].dot ) KeepGo = true ;
      LPinfo.erase( LPinfo.end() - 1 ) ;
    } // if
    else { 
      if ( !LPinfo[LPinfo.size() - 1].dot ) KeepGo = true ;   
      LPinfo.erase( LPinfo.end() - 1 ) ;
    } // else 
 
    if ( pos == NULL && KeepGo ) { 
      pos = new Node ;
      ParenNode->right = pos ;      
      pos->parent = ParenNode ;
      pos->left = NULL ;
      pos->right = NULL ;
      pos->name = "nil" ;
      pos->type = "NIL" ;
    } // else if

   
    dot = false ;
    count = count + 1 ;
    BuildTree( workspace, head, ParenNode, count, dot, LPinfo ) ;

  } // else if
  else {
    if ( workspace[count].type == "QUOTE" ) { 
      if ( count > 0 && ( workspace[count - 1].type == "QUOTE" || dot ) ) {
      	pos = new Node ;
        if ( pos == ParenNode->left ) ParenNode->left = pos ;
        else ParenNode->right = pos ;

        pos->left = NULL ;
        pos->right = NULL ;
        pos->parent = ParenNode ;
        pos->name = "\0" ;
        ParenNode = pos ;
        pos = ParenNode->left ;      
      } // if
      else if ( count > 0 && !dot && workspace[count - 1].type != "QUOTE" ) { //現在這個是QUOTE 
      	//先新增節點，建立樹(往下走一層 
        if ( pos == ParenNode->left ) {
          pos = new Node ;
          ParenNode->left = pos ;
          pos->left = NULL ;
          pos->right = NULL ;
          pos->parent = ParenNode ;
          pos->name = "\0" ;
          ParenNode = pos ;
          pos = ParenNode->left ; 
        } // if 
        else {
          pos = new Node ;
          ParenNode->right = pos ;
          pos->left = NULL ;
          pos->right = NULL ;
          pos->parent = ParenNode ;
          pos->name = "\0" ;
          ParenNode = pos ;

          ParenNode->left = new Node ;          
          pos = ParenNode->left ;
          pos->left = NULL ;
          pos->right = NULL ;
          pos->parent = ParenNode ;
          pos->name = "\0" ;
          ParenNode = pos ;         
        } // else     
      } // else if

      pos = new Node ;    
      ParenNode->left = pos ; 
      pos->parent = ParenNode ;
      pos->left = NULL ;
      pos->right = NULL ;
      pos->name = workspace[count].name ;
      pos->type = workspace[count].type ;

      ParenNode->right = new Node ;
      pos = ParenNode->right ;
      pos->parent = ParenNode ;
      pos->left = NULL ;
      pos->right = new Node ;
      pos->name = "\0" ;
      ParenNode = pos ;  

      pos = ParenNode->right ;
      pos->parent = ParenNode ;
      pos->left = NULL ;
      pos->right = NULL ;
      pos->name = "nil" ;
      pos->type = "NIL" ;

      dot = false ;
      count = count + 1 ;
      BuildTree( workspace, head, ParenNode, count, dot, LPinfo ) ;
    } // if
    else { //放資料 
      if ( pos == ParenNode->left || dot ) {
        pos = new Node ;
        if ( dot ) ParenNode->right = pos ;  
        else ParenNode->left = pos ;
          
        pos->parent = ParenNode ;
        pos->left = NULL ;
        pos->right = NULL ;
        pos->name = workspace[count].name ;
        pos->type = workspace[count].type ;
      } // if
      else {
        pos = new Node ;
        ParenNode->right = pos ;
        pos->parent = ParenNode ;
        pos->left = new Node ;
        pos->right = NULL ;
        pos->name = "\0" ;
        ParenNode = pos ;

        pos = pos->left ;
        pos->parent = ParenNode ;
        pos->left = NULL ;
        pos->right = NULL ;
        pos->name = workspace[count].name ;
        pos->type = workspace[count].type ;

      } // else if

      count = count + 1 ;
      BuildTree( workspace, head, ParenNode, count, dot, LPinfo ) ;
    } // else 
  } // else 
} // BuildTree()
*/
void GetContent( NodePtr head, vector<NodePtr> & FuncNodes, int & error ){
  //收集該function裡面的節點 
  Node * temp = NULL ;
  for ( temp = head->right ; temp->right != NULL && error == 0 ; temp = temp->right ) {
  	
    if ( temp->right != NULL && ( temp->right->name == "nil" || temp->right->name == "\0" ) ) { 
      FuncNodes.push_back( temp->left ); 
    } // if
    else if ( temp->right != NULL && temp->right->name != "\0" ) { // EX:(cons 3 . 5) 
      error = 7 ;
    } // else if
  } // for
} // GetContent()

Node * CopyNode( Node * head ) {
  if ( head == NULL ) return NULL ; 
  else {
    Node * temp = new Node  ;
    temp->parent = NULL ;
    temp->name = head->name ;
    temp->type = head->type ;
    temp->parameters = head->parameters ;
    temp->function = head->function ;
    
    Node * Left = CopyNode( head->left ) ;
    temp->left =  Left ;
    if ( Left != NULL ) Left->parent = temp ;
    
    Node * Right =  CopyNode( head->right ) ;
    temp->right = Right ;
    if ( Right != NULL ) Right->parent = temp ;
    
    return temp ;
  } // else 
} // CopyNode()

NodePtr Cons( Node * head, string & FuncName, vector<NodePtr> FuncNodes, int & error, vector<Table> local_table ){
  string FuncType = "\0" ; 
  
  if( FuncNodes.size() == 2 ){
  	Node * temp = new Node ;
    temp->name = "\0" ;
    temp->parent = NULL ;
    
    //cout << "CONS--------------------------1" << endl ;
    Node * Left = ProcessFunc( FuncNodes[0], FuncName, error, local_table, FuncType ) ; //會retrun一個節點 
    if ( error == 0 ){
      //如oint果左節點工作成功，就copy下來 
      temp->left = CopyNode( Left ) ; 
      temp->left->parent = temp ;
	} // if
	else  return Left ; //如oint果這個節點的工作不正常就return掉
	//cout << "CONS--------------------------2" << endl ;
    Node * Right = ProcessFunc( FuncNodes[1], FuncName, error, local_table, FuncType ) ;
    if ( error == 0 ) {
      //如oint果右節點工作成功，就copy下來  
      temp->right = CopyNode( Right )  ;   
      temp->right->parent = temp ;	
	} // if
	else return Right ; //如oint果這個節點的工作不正常就return掉 
	//cout << "CONS--------------------------3" << endl ;
    return temp ;
  } // if
  else {
  	error = 1 ;
  	return head ;
  } // else
  
} // Cons()

NodePtr List( Node * head, string & FuncName, vector<NodePtr> FuncNodes, int & error, vector<Table> local_table ){
  string FuncType = "\0" ;
  if( FuncNodes.size() > 0 ){
  	NodePtr temp = new Node ;
  	temp->name = "\0" ;
  	temp->left = NULL ;
    temp->right = NULL ;
  	temp->parent = NULL ; 
    
    Node *tail = temp ;
    for( int i = 0 ; i < FuncNodes.size() ; i++ ){
      Node *node ;
      
      node = ProcessFunc( FuncNodes[i], FuncName, error, local_table, FuncType ) ;
      //cout << node->name << endl;
      tail->left = CopyNode( node ) ;
      //cout << "*************************" << endl;
      tail->left->parent = tail ;
      
      tail->right = new Node ;
      tail->right->parent = tail ;
      tail = tail->right ;
      tail->name = "\0" ;
      tail->left = NULL ;
      tail->right = NULL ;
	} //for
	
	if( error == 0 ) tail->name = "nil" ; //代表做完了
	else return tail->parent->left ; //有錯的話回傳上一個節點
	   
    return temp ;
  } // if
  else{
  	//cout << "*****************" << endl;
  	NodePtr temp = new Node ;
  	temp->name = "nil" ;
  	temp->left = NULL ;
    temp->right = NULL ;
  	temp->parent = NULL ; 
  	temp->type = "NIL" ;
  	return temp ;
  } // else

} // List()

NodePtr Quote( Node * head ) {
  Node * temp = CopyNode( head->right->left ) ;
  return temp ; 
} // Quote()

NodePtr Car( Node * head, string & FuncName, vector<NodePtr> FuncNodes, int & error, vector<Table> local_table ){
  string FuncType = "\0" ;
  if( FuncNodes.size() == 1 ){
  	Node *temp = ProcessFunc( FuncNodes[0], FuncName, error, local_table, FuncType ) ;
  	Node *copied = CopyNode( temp ) ;
  	
  	if( error == 0 ){
  	  if( copied->left == NULL && copied->right == NULL ) {
  	    FuncName = "car", error = 3 ;
  	  	return copied ;
	  } // if
  	  else return copied->left ;
	} // if
	else return copied ;
	
  } // if
  else {
  	error = 1 ;
  	return head ;
  } // else
} // Car()

NodePtr Cdr( Node * head, string & FuncName, vector<NodePtr> FuncNodes, int & error, vector<Table> local_table ){
  string FuncType = "\0" ;
  if( FuncNodes.size() == 1 ){
  	Node *node = ProcessFunc( FuncNodes[0], FuncName, error, local_table, FuncType ) ;
  	Node *temp = CopyNode( node ) ;
  	
  	if( error == 0 ){
  	  if( temp->left == NULL && temp->right == NULL ) {
  	  	FuncName = "cdr", error = 3 ;
  	  	return temp ;
	  } // if
  	  else return temp->right ;
	} // if
	else return temp ;
  } // if
  else {
  	error = 1 ;
  	return head ;
  } // else
} // Cdr()

NodePtr IsAtom( Node * head, string & FuncName, vector<NodePtr> FuncNodes, int & error , vector<Table> local_table ){
  string FuncType = "\0" ;
  if( FuncNodes.size() == 1 ){
  	Node *node = ProcessFunc( FuncNodes[0], FuncName, error, local_table, FuncType ) ;
  	if( error == 0 ){
  	  Node *temp = new Node ;
  	  temp->left = NULL ;
  	  temp->right = NULL ;
  	  temp->parent = NULL ;
	   
  	  if( node->left == NULL && node->right == NULL ) temp->name = "#t", temp->type = "SYMBOL" ;
	  else temp->name = "nil", temp->type = "NIL" ;
	  
	  return temp ;
	} // if
	else return node ;
  	
  } // if
  else {
    error = 1 ;
  	return head ;
  } // else
} // IsAtom()

NodePtr IsPair( Node * head, string & FuncName, vector<NodePtr> FuncNodes, int & error, vector<Table> local_table ){
  string FuncType = "\0" ;
  if( FuncNodes.size() == 1 ){
  	Node *node = ProcessFunc( FuncNodes[0], FuncName, error, local_table, FuncType ) ;
  	if( error == 0 ){
  	  Node *temp = new Node ;
  	  temp->left = NULL ;
  	  temp->right = NULL ;
  	  temp->parent = NULL ;
		 
  	  if( node->left == NULL && node->right == NULL ) temp->name = "nil", temp->type = "NIL" ;
	  else temp->name = "#t", temp->type = "SYMBOL" ;
	  
	  return temp ;
	} // if
	else return node ;
  } // if
  else {
  	error = 1 ;
  	return head ;
  } // else
}

NodePtr IsList( Node * head, string & FuncName, vector<NodePtr> FuncNodes, int & error, vector<Table> local_table ){
  string FuncType = "\0" ;
  //cout << "***************" << endl;
  if( FuncNodes.size() == 1 ){
  	Node * node = ProcessFunc( FuncNodes[0], FuncName, error, local_table, FuncType ) ;
  	//cout << "**************" << endl;
  	if( error == 0 ){
  	  Node *temp = new Node ;
  	  temp->left = NULL ;
  	  temp->right = NULL ;
  	  temp->parent = NULL ;
  	  
  	  if( node->left == NULL && node->right == NULL ) temp->name = "nil", temp->type = "NIL" ;
  	  else{
  	    Node * tail = NULL ;
  	    //cout << node->name << endl;
  	    for( tail = node ; tail->right != NULL ; tail = tail->right ){
  	      //cout << "*********" << endl ;
  	      ;
		} // for
		//cout << "END***********" << endl;
		//if( tail == NULL ) cout << "..........." << endl;
		if( tail->name == "nil" ) temp->name = "#t", temp->type = "SYMBOL" ;
		else temp->name = "nil", temp->type = "NIL" ;
      } // if
      //cout << "*********" << endl;
      return temp ;
	} // if
	else return node ;
  } // if
  else {
  	error = 1 ;
  	return head ;
  } // else
} // IsList()

NodePtr IsNull( Node * head, string & FuncName, vector<NodePtr> FuncNodes, int & error, vector<Table> local_table ){
  string FuncType = "\0" ;
  if( FuncNodes.size() == 1 ){
  	Node * node = ProcessFunc( FuncNodes[0], FuncName, error, local_table, FuncType ) ;
  	if( error == 0 ){
  	  Node *temp = new Node ;
  	  temp->left = NULL ;
  	  temp->right = NULL ;
  	  temp->parent = NULL ;
  	  
  	  if( node->left == NULL && node->right == NULL && node->name == "nil" )
  	    temp->name = "#t", temp->type = "SYMBOL" ;
  	  else temp->name = "nil", temp->type = "NIL" ;
  	  return temp ;
	} // if
	else return node ;
  } // if
  else {
  	error = 1 ;
  	return head ;
  } // else
} // IsNULL() 

NodePtr IsInt( Node * head, string & FuncName, vector<NodePtr> FuncNodes, int & error, vector<Table> local_table ){
  string FuncType = "\0" ;
  if( FuncNodes.size() == 1 ){
  	Node * node = ProcessFunc( FuncNodes[0], FuncName, error, local_table, FuncType ) ;
  	if( error == 0 ){
  	  Node * temp = new Node ;
  	  temp->left = NULL ;
  	  temp->right = NULL ;
  	  temp->parent = NULL ;
  	  if( node->left == NULL && node->right == NULL && node->type == "INT" ) temp->name = "#t", temp->type = "SYMBL" ;
  	  else temp->name = "nil", temp->type = "NIL" ;
  	  
  	  return temp ;
	} // if
	else return node ;
  } // if
  else {
  	error = 1 ;
  	return head ;
  } // else

} // IsInt()

NodePtr IsReal( Node * head, string & FuncName, vector<NodePtr> FuncNodes, int & error, vector<Table> local_table ){
  string FuncType = "\0" ;
  if( FuncNodes.size() == 1 ){
  	Node * node = ProcessFunc( FuncNodes[0], FuncName, error, local_table, FuncType ) ;
  	if( error == 0 ){
  	  Node * temp = new Node ;
  	  temp->left = NULL ;
  	  temp->right = NULL ;
  	  temp->parent = NULL ;
  	  
  	  if( node->left == NULL && node->right == NULL ){
  	    if( node->type == "INT" || node->type == "FLOAT" ) temp->name = "#t", temp->type = "SYMBOL" ;
		else temp->name = "nil", temp->type = "NIL" ;
	  } // if
	  else temp->name = "nil", temp->type = "NIL" ;
	  
	  return temp ;
	} // if
	else return node ;
  } // if
  else {
  	error = 1 ;
  	return head ;
  } // else

} // IsReal()

NodePtr IsNum( Node * head, string & FuncName, vector<NodePtr> FuncNodes, int & error, vector<Table> local_table ){
  string FuncType = "\0" ;
  if( FuncNodes.size() == 1 ){
  	Node * node = ProcessFunc( FuncNodes[0], FuncName, error, local_table, FuncType ) ;
  	if( error == 0 ){
  	  Node * temp = new Node ;
  	  temp->left = NULL ;
  	  temp->right = NULL ;
  	  temp->parent = NULL ;
  	  if( node->left == NULL && node->right == NULL ){
  	    if( node->type == "INT" || node->type == "FLOAT" ) temp->name = "#t", temp->type = "SYMBOL" ;
  	    else temp->name = "nil", temp->type = "SYMBOL" ;
	  } // if
	  else temp->name = "nil", temp->type = "NIL" ;
	  return temp ;
    } // if
    else return node ;
  } // if
  else {
  	error = 1 ;
  	return head ;
  } // else
}

NodePtr IsString( Node * head, string & FuncName, vector<NodePtr> FuncNodes, int & error, vector<Table> local_table ){
  string FuncType = "\0" ;
  if( FuncNodes.size() == 1 ){
  	Node * node = ProcessFunc( FuncNodes[0], FuncName, error, local_table, FuncType ) ;
  	if( error == 0 ){
  	  Node * temp = new Node ;
  	  temp->left = NULL ;
  	  temp->right = NULL ;
  	  temp->parent = NULL ;
  	  if( node->left == NULL && node->right == NULL ){
  	  	if( node->type == "STRING" || node->type == "NIL" ) temp->name = "#t", temp->type = "SYMBOL" ;
  	  	else temp->name = "nil", temp->type = "NIL" ;
	  } // if
	  else temp->name = "nil", temp->type = "NIL" ;
	  return temp ;
	} // if
	else return node ;
  } // if
  else {
  	error = 1 ;
  	return head ;
  } // else

} // IsStirng()

NodePtr IsBool( Node * head, string & FuncName, vector<NodePtr> FuncNodes, int & error, vector<Table> local_table ){
  string FuncType = "\0" ;
  if( FuncNodes.size() == 1 ){
  	Node * node = ProcessFunc( FuncNodes[0], FuncName, error, local_table, FuncType ) ;
  	if( error == 0 ){
  	  Node * temp = new Node ;
  	  temp->left = NULL ;
  	  temp->right = NULL ;
  	  temp->parent = NULL ;
  	  if( node->left == NULL && node->right == NULL ){
  	  	if( node->name == "#t" || node->type == "NIL" ) temp->name = "#t", temp->type = "SYMBOL" ;
  	  	else temp->name = "nil", temp->type = "NIL" ;
	  } // if
	  else temp->name = "nil", temp->type = "NIL" ;
	  return temp ;
	} // if
	else return node ;
  } // if
  else {
  	error = 1 ;
  	return head ;
  } // else

} // IsStirng()

NodePtr IsSymbol( Node * head, string & FuncName, vector<NodePtr> FuncNodes, int & error, vector<Table> local_table ){
  string FuncType = "\0" ;
  if( FuncNodes.size() == 1 ){
  	Node * node = ProcessFunc( FuncNodes[0], FuncName, error, local_table, FuncType ) ;
  	if( error == 0 ){
  	  Node * temp = new Node ;
  	  temp->left = NULL ;
  	  temp->right = NULL ;
  	  temp->parent = NULL ;
  	  if( node->left == NULL && node->right == NULL && node->type == "SYMBOL" ) 
		temp->name = "#t", temp->type = "SYMBOL" ;
	  else temp->name = "nil", temp->type = "NIL" ;
	  return temp ;
	} // if
	else return node ;
  } // if
  else {
  	error = 1 ;
  	return head ;
  } // else
} // IsStirng()

void ftoa( float num, int precision, char *str, string & type ) {
  char temp[20] ;
  bool isFloat = false ;
  sprintf( temp, "%g", num ) ;
  string ttemp = temp ;
  for( int i = 0 ; i < ttemp.size() ; i++ ){
    if( ttemp[i] == '.' ) isFloat = true ; 
  } // for
	
  if( isFloat ) type = "FLOAT", sprintf( str, "%.*f", precision, num ) ;
  else type = "INT", sprintf( str, "%g", num ) ;
} // ftoa

NodePtr Add( Node * head, string &FuncName, vector<NodePtr> FuncNodes, int & error, vector<Table> local_table ){
  string FuncType = "\0" ;
  if( FuncNodes.size() < 2 ){
  	error = 1 ;
  	return head ;
  } // if
  else{
  	Node * node ;
  	float sum = 0 ;
  	bool IfFloat = false ;
  	for( int i = 0 ; i < FuncNodes.size() ; i++ ){
  	  node = ProcessFunc( FuncNodes[i], FuncName, error, local_table, FuncType ) ;
  	  if( error == 0 ){
  	  	if( i == 0 ){
  	  	  	if( node->type == "FLOAT" ) IfFloat = true ;
  	  	  	float num = atof( node->name.c_str() ) ;
  	  	    sum = num ;
		} // if
  	  	else if( node->left == NULL && node->right == NULL && node->type == "INT" ){
  	  	  float num = atof( node->name.c_str() ) ;
  	  	  sum = sum + num ;
		} // if
		else if( node->left == NULL && node->right == NULL && node->type == "FLOAT" ){
		  IfFloat = true ;
		  float num = atof( node->name.c_str() ) ;
  	  	  sum = sum + num ;
		} // else if
		else {
		  FuncName = "+", error = 3 ;
		  return node ;
		} // else
	  } // if
	  else return node ;
	  
	} // for
	
	char ans[20] ;
	Node *temp = new Node ;
	temp->left = NULL ;
	temp->right = NULL ;
	temp->parent = NULL ;
	
	sum = Rounding( sum, 3 ) ;
	if( IfFloat ) temp->type = "FLOAT", sprintf( ans, "%.*f", 3, sum ) ;
	else{
	  int tsum = sum ;
	  temp->type = "INT", sprintf( ans, "%d", tsum ) ;
	} // else
	temp->name = ans ;
	return temp ;
  } // else
} // Add()

NodePtr Minus( Node * head, string &FuncName, vector<NodePtr> FuncNodes, int & error, vector<Table> local_table ){
  string FuncType = "\0" ;
  if( FuncNodes.size() < 2 ) {
  	error = 1 ;
  	return head ;
  } // if
  else{
  	Node * node ;
  	float sum = 0 ;
  	bool IfFloat = false ;
  	for( int i = 0 ; i < FuncNodes.size() ; i++ ){
  	  node = ProcessFunc( FuncNodes[i], FuncName, error, local_table, FuncType ) ;
  	  if( error == 0 ){
  	  	if( i == 0 ){
  	  	  	if( node->type == "FLOAT" ) IfFloat = true ;
  	  	  	float num = atof( node->name.c_str() ) ;
  	  	    sum = num ;
		} // if
  	  	else if( node->left == NULL && node->right == NULL && node->type == "INT" ){
  	  	  float num = atof( node->name.c_str() ) ;
  	  	  sum = sum - num ;
		} // if
		else if( node->left == NULL && node->right == NULL && node->type == "FLOAT" ){
		  IfFloat = true ;
		  float num = atof( node->name.c_str() ) ;
  	  	  sum = sum - num ;
		} // else if
		else {
		  FuncName = "-", error = 3 ;
		  return node ;
		} // else
	  } // if
	  else return node ;
	} // for
	
	char ans[20] ;
	Node *temp = new Node ;
	temp->left = NULL ;
	temp->right = NULL ;
	temp->parent = NULL ;
	//cout << sum << endl;
	sum = Rounding( sum, 3 ) ;
	if( IfFloat ) temp->type = "FLOAT", sprintf( ans, "%.*f", 3, sum ) ;
	else{
	  int tsum = sum ;
	  temp->type = "INT", sprintf( ans, "%d", tsum ) ;
	} // else
	temp->name = ans ;
	return temp ;
  } // else
} // Minus()

NodePtr Multiple( Node * head, string &FuncName, vector<NodePtr> FuncNodes, int & error, vector<Table> local_table ){
  string FuncType = "\0" ;
  if( FuncNodes.size() < 2 ) {
  	error = 1 ;
  	return head ;
  } // if
  else{
  	Node * node ;
  	float sum = 0 ;
  	bool IfFloat = false ;
  	for( int i = 0 ; i < FuncNodes.size() ; i++ ){
  	  node = ProcessFunc( FuncNodes[i], FuncName, error, local_table, FuncType ) ;
  	  if( error == 0 ){
  	  	if( node->left == NULL && node->right == NULL && ( node->type == "INT" || node->type == "FLOAT" ) ){
  	  	  if( i == 0 ){
  	  	  	if( node->type == "FLOAT" ) IfFloat = true ;
  	  	  	float num = atof( node->name.c_str() ) ;
  	  	    sum = num ;
		  } // if
		  else if( node->type == "INT" ){
  	  	    float num = atof( node->name.c_str() ) ;
  	  	    sum = sum * num ;
	  	  } // else if
		  else if( node->type == "FLOAT" ){
		    IfFloat = true ;
		    float num = atof( node->name.c_str() ) ;
  	  	    sum = sum * num ;
		  } // else if
	    } // if
		else {
		  FuncName = "*", error = 3 ;
		  return node ;
		} // else
	  } // if
	  else return node ;
	} // for
	
	char ans[20] ;
	Node *temp = new Node ;
	temp->left = NULL ;
	temp->right = NULL ;
	temp->parent = NULL ;
	
	sum = Rounding( sum, 3 ) ;
	if( IfFloat ) temp->type = "FLOAT", sprintf( ans, "%.*f", 3, sum ) ;
	else{
	  int tsum = sum ;
	  temp->type = "INT", sprintf( ans, "%d", tsum ) ;
	} // else
	temp->name = ans ;
	
	return temp ;
  } // else
} // Multiple()

NodePtr Division( Node * head, string &FuncName, vector<NodePtr> FuncNodes, int & error, vector<Table> local_table ){
  string FuncType = "\0" ;
  if( FuncNodes.size() < 2 ){
  	error = 1 ;
  	return head ;
  } // if
  else{
  	Node * node ;
  	float sum = 0 ;
  	bool IfFloat = false ;
  	for( int i = 0 ; i < FuncNodes.size() ; i++ ){
  	  node = ProcessFunc( FuncNodes[i], FuncName, error, local_table, FuncType ) ;
  	  if( error == 0 ){
  	  	if( node->left == NULL && node->right == NULL && ( node->type == "INT" || node->type == "FLOAT" ) ){
  	  	  if( i == 0 ){
  	  	  	if( node->type == "FLOAT" ) IfFloat = true ;
  	  	  	float num = atof( node->name.c_str() ) ;
  	  	    sum = num ;
		  } // if
		  else if( node->name == "0" ) {
		  	FuncName = "/", error = 4 ;
		  	return node ;
		  } // else if
		  else if( node->type == "INT" ){
  	  	    float num = atof( node->name.c_str() ) ;
  	  	    sum = sum / num ;
	  	  } // else if
		  else if( node->type == "FLOAT" ){
		    IfFloat = true ;
		    float num = atof( node->name.c_str() ) ;
  	  	    sum = sum / num ;
		  } // else if
	    } // if
		else {
		  FuncName = "/", error = 3 ;
		  return node ;
		} // else
	  } // if
	  else return node ;
	} // for
	
	char ans[20] ;
	Node *temp = new Node ;
	temp->left = NULL ;
	temp->right = NULL ;
	temp->parent = NULL ;
	//cout << sum << endl;
	sum = Rounding( sum, 3 ) ;
	if( IfFloat ) temp->type = "FLOAT", sprintf( ans, "%.*f", 3, sum ) ;
	else{
	  int tsum = sum ;
	  temp->type = "INT", sprintf( ans, "%d", tsum ) ;
	} // else
	temp->name = ans ;
	
	return temp ;
  } // else
} // Division()

NodePtr NOT( Node * head, string &FuncName, vector<NodePtr> FuncNodes, int & error, vector<Table> local_table ){
  string FuncType = "\0" ;
  if( FuncNodes.size() == 1 ){
  	Node * node = ProcessFunc( FuncNodes[0], FuncName, error, local_table, FuncType ) ;
  	if( error == 0 ){
  	  Node * temp = new Node ;
  	  temp->left = NULL ;
  	  temp->right = NULL ;
  	  temp->parent = NULL ;
  	  if( node->type == "NIL" ) temp->name = "#t", temp->type = "SYMBOL" ;
	  else temp->name = "nil", temp->type = "NIL" ;
	  return temp ;
	} // if
	else return node ;
  } // if
  else {
  	error = 1 ;
  	return head ;
  } // else
} // NOT()

NodePtr AND( Node * head, string &FuncName, vector<NodePtr> FuncNodes, int & error, vector<Table> local_table ){
  string FuncType = "\0" ;
  if( FuncNodes.size() < 2 ){
  	error = 1 ;
  	return head ;
  } // if
  else{
  	Node * node ;
  	for( int i = 0 ;  i < FuncNodes.size() ; i++ ){
  	  node = ProcessFunc( FuncNodes[i], FuncName, error, local_table, FuncType ) ;
  	  if( error != 0 ) return node ;
	  else if( node->type == "NIL" ) return node  ; 
	} // for
	
	return node ; //回傳最後一個 
  } // else
} // AND()

NodePtr OR( Node * head, string &FuncName, vector<NodePtr> FuncNodes, int & error, vector<Table> local_table ){
  string FuncType = "\0" ;
  if( FuncNodes.size() < 2 ) {
  	error = 1 ;
  	return head ;
  } // if
  else{
  	Node * node ;
  	for( int i = 0 ;  i < FuncNodes.size() ; i++ ){
  	  node = ProcessFunc( FuncNodes[i], FuncName, error, local_table, FuncType ) ;
  	  if( error != 0 ) return node ;
	  else if( node->type != "NIL" ) return node  ; 
	} // for
	
	return node ; //回傳最後一個 
  } // else
} // OR()
/*Mine
NodePtr Bigger( Node * head, string &FuncName, vector<NodePtr> FuncNodes, int & error, vector<Table> local_table ) {  
  string FuncType = "\0" ;
  cout << "==========================" << FuncNodes.size() << endl;
  if( FuncNodes.size() < 2 ) {
  	error = 1 ;
  	return head ;
  } // if
  else{
  	Node * node ;
  	float num = 0 ;
  	bool ANS = true ;
  	//cout << "--------------" << FuncNodes.size() << endl;
  	for( int i = 0 ; i < FuncNodes.size() ; i++ ){
  	  node = ProcessFunc( FuncNodes[i], FuncName, error, local_table, FuncType ) ;
  	  cout << i << " " << node->name << endl;
  	  if( error == 0 ) {
  	  	//cout << node->name << endl;
  	    if( node->left == NULL && node->right == NULL && ( node->type == "INT" || node->type == "FLOAT" ) ){
  	      //cout << "BIGGER--------------------" << node->name << endl;
  	      if( i == 0 ) num = atof( node->name.c_str() ) ;
  	      else{
  	      	float temp = atof( node->name.c_str() ) ;
  	        if( num > temp ) num = temp ;
  	        else ANS = false ;
		  } // else
		} // if
		else {
		  FuncName = ">", error = 3 ;
		  return node ;
		} // else
	  } // if
	  else return node ;
	} // for
	
	Node * ans = new Node ;
	ans->left = NULL ;
	ans->right = NULL ;
	ans->parent = NULL ;
	
	if( ANS ) ans->name = "#t", ans->type = "SYMBOL" ;
	else ans->name = "nil", ans->type = "NIL" ;
	//cout << "BIGGER--------------" << ans->name << endl;
	return ans ;
  } // else
} // Bigger
*/

NodePtr Bigger( Node * head, string & Fname, vector<NodePtr> N_list, int & err, vector<Table> local_chart ) {
                
  string func_num = "\0" ;
  float now = 0.0 ; // 現在手上的數字 
  bool result = false ; // 是否符合要求 
  //cout << "=============================" << N_list.size() << endl;
  Node * tmp = new Node ;
  tmp->parent = NULL ;
  tmp->left = NULL ;
  tmp->right = NULL ;  
  if ( N_list.size() < 2 ) {
    err = 1 ;
    return head ;
  } // if
  else {
    Node * arg = NULL ;
    for ( int i = 0; i <= N_list.size() - 1 && result == false ; i++ ) { // 檢查type
      arg = ProcessFunc( N_list[i], Fname, err, local_chart, func_num ) ;
      if ( err != 0 ) {
        return arg ;
      } // if
      else if ( arg->left == NULL && arg->right == NULL && ( arg->type == "INT" || arg->type == "FLOAT" ) ) {
        ;
      } // else if
      else {
        err = 3 ;
        Fname = ">" ;
        return arg ;  
      } // else
    } // for

    for ( int i = 0; i <= N_list.size() - 1 && result == false ; i++ ) { 
      arg = ProcessFunc( N_list[i], Fname, err, local_chart, func_num ) ;
      //cout << i << " " << arg->name << endl;
      if ( err != 0 ) {
        return arg ;
      } // if
      else {
        if ( arg->left == NULL && arg->right == NULL && ( arg->type == "INT" || arg->type == "FLOAT" ) ) {
          // 要的是樹葉
          int n = arg->name.length() + 1 ;
          char char_array[n] ;
          strcpy( char_array, arg->name.c_str() ) ;
          if ( i == 0 ) { // 第一筆 
            now = atof( char_array ) ; 
          } // if
          else {
            int n = arg->name.length() + 1 ;
            char char_array[n] ;
            strcpy( char_array, arg->name.c_str() ) ;
            if ( now > atof( char_array ) ) {
              now = atof( char_array ) ;
            } // if
            else {
              result = true ;
            } // else
          } // else
        } // if
        else { // 來了樹 
          err = 3 ;
          Fname = ">" ;
          return arg ;        
        } // else
      } // else
    } // for
    
    if ( result == false ) {
      tmp->name = "#t" ;
      tmp->type = "SYMBOL" ;
      //cout  << "BIGGER-------------------" << tmp->token << endl;
      return tmp ;     
    } // if
    else {
      tmp->name = "nil" ;
      tmp->type = "NIL" ;
      return tmp ;    
    } // else
  } // else
} // Bigger()


NodePtr BiggerEqual( Node * head, string &FuncName, vector<NodePtr> FuncNodes, int & error, vector<Table> local_table ) {
  string FuncType = "\0" ;
  if( FuncNodes.size() < 2 ) {
  	error = 1 ;
  	return head ;
  } // if
  else{
  	Node * node ;
  	float num = 0 ;
  	bool ANS = true ;
  	for( int i = 0 ; i < FuncNodes.size() ; i++ ){
  	  node = ProcessFunc( FuncNodes[i], FuncName, error, local_table, FuncType ) ;
  	  if( error == 0 ) {
  	    if( node->left == NULL && node->right == NULL && ( node->type == "INT" || node->type == "FLOAT" ) ){
  	      if( i == 0 ) num = atof( node->name.c_str() ) ;
  	      else{
  	      	float temp = atof( node->name.c_str() ) ;
  	        if( num >= temp ) num = temp ;
  	        else ANS = false ;
		  } // else
		} // if
		else {
		  FuncName = ">=", error = 3 ;
		  return node ;
		} // else
	  } // if
	  else return node ;
	} // for
	
	Node * ans = new Node ;
	ans->left = NULL ;
	ans->right = NULL ;
	ans->parent = NULL ;
	if( ANS ) ans->name = "#t", ans->type = "SYMBOL" ;
	else ans->name = "nil", ans->type = "NIL" ;
	return ans ;
  } // else
} // BiggerEqual

NodePtr Less( Node * head, string &FuncName, vector<NodePtr> FuncNodes, int & error, vector<Table> local_table ) {
  string FuncType = "\0" ;
  if( FuncNodes.size() < 2 ) {
  	error = 1 ;
  	return head ;
  } // if
  else{
  	Node * node ;
  	float num = 0 ;
  	bool ANS = true ;
  	for( int i = 0 ; i < FuncNodes.size() ; i++ ){
  	  node = ProcessFunc( FuncNodes[i], FuncName, error, local_table, FuncType ) ;
  	  if( error == 0 ) {
  	    if( node->left == NULL && node->right == NULL && ( node->type == "INT" || node->type == "FLOAT" ) ){
  	      if( i == 0 ) num = atof( node->name.c_str() ) ;
  	      else{
  	      	float temp = atof( node->name.c_str() ) ;
  	        if( num < temp ) num = temp ;
  	        else ANS = false ;
		  } // else
		} // if
		else {
		  FuncName = "<", error = 3 ;
		  return node ;
		} // else
	  } // if
	  else return node ;
	} // for
	
	Node * ans = new Node ;
	ans->left = NULL ;
	ans->right = NULL ;
	ans->parent = NULL ;
	if( ANS ) ans->name = "#t", ans->type = "SYMBOL" ;
	else ans->name = "nil", ans->type = "NIL" ;
	return ans ;
  } // else
} // Less

NodePtr LessEqual( Node * head, string &FuncName, vector<NodePtr> FuncNodes, int & error, vector<Table> local_table ) {
  string FuncType = "\0" ;
  if( FuncNodes.size() < 2 ) {
  	error = 1 ;
  	return head ;
  } // if
  else{
  	Node * node ;
  	float num = 0 ;
  	bool ANS = true ;
  	for( int i = 0 ; i < FuncNodes.size() ; i++ ){
  	  node = ProcessFunc( FuncNodes[i], FuncName, error, local_table, FuncType ) ;
  	  if( error == 0 ) {
  	    if( node->left == NULL && node->right == NULL && ( node->type == "INT" || node->type == "FLOAT" ) ){
  	      if( i == 0 ) num = atof( node->name.c_str() ) ;
  	      else{
  	      	float temp = atof( node->name.c_str() ) ;
  	        if( num <= temp ) num = temp ;
  	        else ANS = false ;
		  } // else
		} // if
		else{
		   FuncName = "<=", error = 3 ;
		   return node ;
		} // else
	  } // if
	  else return node ;
	} // for
	
	Node * ans = new Node ;
	ans->left = NULL ;
	ans->right = NULL ;
	ans->parent = NULL ;
	if( ANS ) ans->name = "#t", ans->type = "SYMBOL" ;
	else ans->name = "nil", ans->type = "NIL" ;
	return ans ;
  } // else
} // LessEqual

NodePtr Equal( Node * head, string &FuncName, vector<NodePtr> FuncNodes, int & error, vector<Table> local_table ) {
  string FuncType = "\0" ;
  if( FuncNodes.size() < 2 ) {
  	error = 1 ;
  	return head ;
  } // if
  else{
  	Node * node ;
  	float num = 0 ;
  	bool ANS = true ;
  	for( int i = 0 ; i < FuncNodes.size() ; i++ ){
  	  node = ProcessFunc( FuncNodes[i], FuncName, error, local_table, FuncType ) ;
  	  if( error == 0 ) {
  	    if( node->left == NULL && node->right == NULL && ( node->type == "INT" || node->type == "FLOAT" ) ){
  	      if( i == 0 ) num = atof( node->name.c_str() ) ;
  	      else{
  	      	float temp = atof( node->name.c_str() ) ;
  	        if( num == temp ) num = temp ;
  	        else ANS = false ;
		  } // else
		} // if
		else {
		  FuncName = "=", error = 3 ;
		  return node ;
		} // else
	  } // if
	  else return node ;
	} // for
	
	Node * ans = new Node ;
	ans->left = NULL ;
	ans->right = NULL ;
	ans->parent = NULL ;
	if( ANS ) ans->name = "#t", ans->type = "SYMBOL" ;
	else ans->name = "nil", ans->type = "NIL" ;
	return ans ;
  } // else
} // Equal

NodePtr StringAppend ( Node * head, string & FuncName, vector<NodePtr> FuncNodes, int & error, vector<Table> local_table ){
  string FuncType = "\0" ;
  if( FuncNodes.size() < 2 ) {
  	error = 1 ;
  	return head ;
  } // if
  else{
  	Node * node ;
  	string ans = "\"" ;
  	for( int i = 0 ;  i < FuncNodes.size() ; i++ ){
  	  node = ProcessFunc( FuncNodes[i], FuncName, error, local_table, FuncType ) ;
  	  if( error == 0 ){
  	  	if( node->left == NULL && node->right == NULL && node->type == "STRING" ){
  	  	  string str = node->name ;
  	  	  str.erase( str.begin() ) ;
  	  	  str.erase( str.end()-1 ) ;
  	  	  ans = ans + str ;
		} // if
  	  	else {
  	  	  FuncName = "string-append", error = 3 ;
  	  	  return node ;
		} // else
	  } // if
	  else return node ;
	} // for
	
	ans =  ans + "\"" ;
	Node * temp = new Node ;
	temp->left = NULL ;
	temp->right = NULL ;
	temp->parent = NULL ;
	temp->name = ans ;
	temp->type = "STRING" ;
	
	if( temp->left != NULL ) cout << "1!!!!!!!!!!!!!!!!!!!!" << endl;
	if( temp->right != NULL ) cout << "2!!!!!!!!!!!!!!!!!!!" << endl;
	return temp ;
  } // else
} // StringAppend ()

NodePtr StringBigger( Node * head, string &FuncName, vector<NodePtr> FuncNodes, int & error, vector<Table> local_table ){
  string FuncType = "\0" ;
  //cout << "---------------------" << FuncNodes.size() << endl;
  if( FuncNodes.size() < 2 ) {
  	error = 1 ;
  	return head ;
  } // if
  else{
  	Node * node ;
  	string temp ;
  	bool ans = true ;
  	for( int i = 0 ; i < FuncNodes.size() ; i++ ){
  	  node = ProcessFunc( FuncNodes[i], FuncName, error, local_table, FuncType ) ;
  	  //cout << i << "**********************" << error << endl;
  	  if( error == 0 ){
  	  	//if( node->left != NULL ) cout << "1-------------------" << endl;
  	  	//if( node->right != NULL ) cout << "2-------------------" << endl;
  	  	
  	  	if( node->left == NULL && node->right == NULL && node->type == "STRING" ){
  	  	  if( i == 0 ) temp = node->name ;
  	  	  else if( temp > node->name ) temp = node->name ;
  	  	  else ans = false ;
  	  	  
  	  	  //cout << i << " " << temp << endl;
		} // if
		else {
		  //cout << "******************" << endl;
		  FuncName = "string>?", error = 3 ;
		  return node ;
		} // else
	  } // if
	  else return node ;
	} // for
	
	Node * tmp = new Node ;
	tmp->left = NULL ;
	tmp->right = NULL ;
	tmp->parent = NULL ;
	if( ans ){
	  tmp->name = "#t" ;
	  tmp->type = "SYMBOL" ;
	} // if
	else{
	  tmp->name = "nil" ;
	  tmp->type = "NIL" ;
	} // else
	return tmp ;
  } //else
} // StringBigger()

NodePtr StringLess( Node * head, string &FuncName, vector<NodePtr> FuncNodes, int & error, vector<Table> local_table ){
  string FuncType = "\0" ;
  if( FuncNodes.size() < 2 ) {
  	error = 1 ;
  	return head ;
  } // if
  else{
  	Node * node ;
  	string temp ;
  	bool ans = true ;
  	for( int i = 0 ; i < FuncNodes.size() ; i++ ){
  	  node = ProcessFunc( FuncNodes[i], FuncName, error, local_table, FuncType ) ;
  	  if( error == 0 ){
  	  	if( node->left == NULL && node->right == NULL && node->type == "STRING" ){
  	  	  if( i == 0 ) temp = node->name ;
  	  	  else if( temp < node->name ) temp = node->name ;
  	  	  else ans = false ;
		} // if
		else {
		  FuncName = "string<?", error = 3 ;
		  return node ;
		} // else
	  } // if
	  else return node ;
	} // for
	
	Node * tmp = new Node ;
	tmp->left = NULL ;
	tmp->right = NULL ;
	tmp->parent = NULL ;
	if( ans ){
	  tmp->name = "#t" ;
	  tmp->type = "SYMBOL" ;
	} // if
	else{
	  tmp->name = "nil" ;
	  tmp->type = "NIL" ;
	} // else
	return tmp ;
  } //else
} // StringLess()

NodePtr StringEqual( Node * head, string &FuncName, vector<NodePtr> FuncNodes, int & error, vector<Table> local_table ){
  string FuncType = "\0" ;
  if( FuncNodes.size() < 2 ) {
  	error = 1 ;
  	return head ;
  } // if
  else{
  	Node * node ;
  	string temp ;
  	bool ans = true ;
  	for( int i = 0 ; i < FuncNodes.size() ; i++ ){
  	  node = ProcessFunc( FuncNodes[i], FuncName, error, local_table, FuncType ) ;
  	  if( error == 0 ){
  	  	if( node->left == NULL && node->right == NULL && node->type == "STRING" ){
  	  	  if( i == 0 ) temp = node->name ;
  	  	  else if( temp == node->name ) temp = node->name ;
  	  	  else ans = false ;
		} // if
		else {
		  FuncName = "string=?", error = 3 ;
		  return node ;
		} // else 
	  } // if
	  else return node ;
	} // for
	
	Node * tmp = new Node ;
	tmp->left = NULL ;
	tmp->right = NULL ;
	tmp->parent = NULL ;
	if( ans ){
	  tmp->name = "#t" ;
	  tmp->type = "SYMBOL" ;
	} // if
	else{
	  tmp->name = "nil" ;
	  tmp->type = "NIL" ;
	} // else
	return tmp ;
  } //else
} // StringEqual()

bool SameNode( Node * node1, Node * node2 ){
  if( node1 == NULL && node2 == NULL ) return true ;
  else{
  	if( node1 == NULL && node2 !=  NULL ) return false ;
  	else if( node1 != NULL && node2 == NULL ) return false ;
  	else if( node1->name != node2->name ) return false ;
  	else return SameNode( node1->left, node2->left ) && SameNode( node1->right, node2->right ) ; 
  } // else 
} //SameNode()

NodePtr IsEqual( Node * head, string &FuncName, vector<NodePtr> FuncNodes, int & error, vector<Table> local_table ){
  string FuncType = "\0" ;
  if( FuncNodes.size() == 2 ){
  	Node * node1, * node2 ;
  	node1 = ProcessFunc( FuncNodes[0], FuncName, error, local_table, FuncType ) ;
  	if( error != 0 ) return node1 ;
  	node2 = ProcessFunc( FuncNodes[1], FuncName, error, local_table, FuncType ) ;
  	if( error != 0 ) return node2 ;
  	
  	Node *temp = new Node ;
  	temp->left = NULL ;
  	temp->right = NULL ;
  	temp->parent = NULL ;
  	if( SameNode( node1, node2 ) ) temp->name = "#t", temp->type = "SYMBOL" ;
  	else temp->name = "nil", temp->type = "NIL" ;
  	
  	return temp ;
  } // if
  else {
  	error = 1 ;
  	return head ;
  } // else
} //IsEqv()

NodePtr IsEqv( Node * head, string &FuncName, vector<NodePtr> FuncNodes, int & error, vector<Table> local_table ){
  string FuncType = "\0" ;
  if( FuncNodes.size() == 2 ){
  	Node * node1, * node2 ;
  	node1 = ProcessFunc( FuncNodes[0], FuncName, error, local_table, FuncType ) ;
  	if( error != 0 ) return node1 ;
  	node2 = ProcessFunc( FuncNodes[1], FuncName, error, local_table, FuncType ) ;
  	if( error != 0 ) return node2 ;
  	
  	Node *temp = new Node ;
  	temp->left = NULL ;
  	temp->right = NULL ;
  	temp->parent = NULL ;
  	if( node1 == node2 ) temp->name = "#t", temp->type = "SYMBOL" ;
  	else if( node1->name == "#t" && node2->name == "#t" ) temp->name = "#t", temp->type = "SYMBOL" ;
  	else if( node1->name == "nil" && node2->name == "nil" ) temp->name = "#t", temp->type = "SYMBOL" ;
  	else if( ( node1->type == "INT" || node1->type == "FLOAT" ) && ( node2->type == "INT" || node2->type == "FLOAT" ) ) {
  	  if( node1->name == node2->name ) temp->name = "#t", temp->type = "SYMBOL" ;
  	  else temp->name = "nil", temp->type = "NIL" ;
	} // else if
  	else temp->name = "nil", temp->type = "NIL" ;
  	
  	return temp ;
  } // if
  else {
  	error = 1 ;
  	return head ;
  } // else
} //IsEqv()

NodePtr Begin( Node * head, string &FuncName, vector<NodePtr> FuncNodes, int & error, vector<Table> local_table ){
  string FuncType = "\0" ;
  if( FuncNodes.size() != 0 ){
  	Node * node, * temp ;
  	for( int i = 0 ; i < FuncNodes.size() ; i++ ){
  	  node = ProcessFunc( FuncNodes[i], FuncName, error, local_table, FuncType ) ;
  	  temp = CopyNode( node ) ;
  	  if( error != 0 ) return node ;
	} // for
	
	return temp ;
  } // if
  else {
  	error = 1 ;
  	return head ;
  } // else
} // Begin


NodePtr If( Node * head, string &FuncName, vector<NodePtr> FuncNodes, int & error, vector<Table> local_table ){
  string FuncType = "\0" ;
  if( FuncNodes.size() == 2 || FuncNodes.size() == 3 ){
  	Node * cond = ProcessFunc( FuncNodes[0], FuncName, error, local_table, FuncType ) ;
  	//cout << "BEGIN IF.........................." << cond->name << " " << cond->function.size() << endl;
  	if( error == 0 ){
  	  if( FuncNodes.size() == 2 ){
  	    if( cond->type == "NIL" ){
  	      error = 5 ;
	  	  return head ;
	    } // if
	    else {
	      Node * node = ProcessFunc( FuncNodes[1], FuncName, error, local_table, FuncType ) ;
  	  	  if( error == 0 ){
  	  	    Node * copy = CopyNode( node ) ;
  	  	    return copy ;
		  } // if
  	  	  else return node ;
	    } //else
	  } // if
	  else{
	    if( cond->type == "NIL" ){
	  	  Node * node = ProcessFunc( FuncNodes[2], FuncName, error, local_table, FuncType ) ;
	  	  Node * copy = CopyNode( node ) ;
	  	  return copy ;
	    } // if
	    else{
	      //cout << "IF..............................." << FuncNodes[1]->name << " " << FuncType << endl; 
	      Node * node = ProcessFunc( FuncNodes[1], FuncName, error, local_table, FuncType ) ;
	      //cout << "MIDDLE IF........................" << node->name << " " << node->function.size() << endl ;
	  	  Node * copy = CopyNode( node ) ;
	  	  //cout << "END IF............................" << copy->name << " " << copy->function.size() << endl;
	  	  return copy ;
	    } // else
	  
	  } // else
	} // if
	else return cond ;
  	
  } // if
  else {
  	error = 1 ;
  	return head ;
  } // else
}

NodePtr Cond( Node * head, string &FuncName, vector<NodePtr> FuncNodes, int & error, vector<Table> local_table ){
  string FuncType = "\0" ;
  if( FuncNodes.size() > 0 ){
  	for( int i = 0 ; i < FuncNodes.size() ; i++ ){ //檢查每一個node是不是樹，而不是葉 
	  												//ex: ( cond #t 3 )
  	  if( FuncNodes[i]->left == NULL && FuncNodes[i]->right == NULL && FuncNodes[i]->name != "\0" ){
  	  	error = 2 ;
  	  	return head ;
	  } // if
  	  else{
  	  	//如oint果是樹也就是list就去檢查裡面有沒有東西 
  	  	vector<NodePtr> part ;
  	  	GetContent( FuncNodes[i], part, error ) ;
  	  	if( part.size() == 0 ) {
  	  	  error = 2 ;
  	  	  return head ;
		} // if
	  } // else
	} // for
	
	//正式開始做 
	Node * ans = NULL ;
	for( int i = 0 ; i < FuncNodes.size() && ans == NULL ; i++ ){
	  //這裡會將條件判斷式處裡
	  Node * cond = ProcessFunc( FuncNodes[i]->left, FuncName, error, local_table, FuncType ) ;
	  //cout << i << " " << cond->name << endl; 
	  if( error == 0 ){ 
	  	if( cond->type != "NIL" || ( i == FuncNodes.size()-1 && FuncNodes[i]->left->name == "else" ) ){
	  		//如果判斷式成立，處裡判斷式裡面的內容 
	  	  //cout << "**************" << endl;
	  	  vector<NodePtr> part ;
	  	  GetContent( FuncNodes[i], part, error ) ;
	  	  
	  	  if( error == 0 ){
	  	    Node * node = NULL ;
	  	    //cout << "******************" << endl;
	  	    for( int j = 0 ; j < part.size() ; j++ ){
	  	      node = ProcessFunc( part[j], FuncName, error, local_table, FuncType ) ;
	  	      if( error != 0 ) return node ;
			} // for
			
			if( ans == NULL ) ans = CopyNode( node ) ;
		  } // if
		  else if( part.size() == 0 ) {
		  	error = 2 ;
		  	return head ;
		  } // else if
		  else return FuncNodes[i] ; // error != 0
		} // if
		else{
		  vector<NodePtr> part ;
          GetContent( FuncNodes[i], part, error ) ; 
          if ( part.size() == 0 ) {
            error = 2 ;
            return head ;  
          } // if
		} // else
	  } // if
	  else{ // error != 0
	  	if( FuncNodes[i]->left->name == "else" && i == FuncNodes.size()-1 ){
	  	  vector<NodePtr> part ;
		  error = 0 ;
	  	  GetContent( FuncNodes[i], part, error ) ;
	  	  if( error == 0 ){
	  	    Node * node = NULL ;
	  	    for( int j = 0 ; j < part.size() ; j++ ){
	  	      node = ProcessFunc( part[j], FuncName, error, local_table, FuncType ) ;
	  	      if( error != 0 ) return node ;
			} // for
			
			if( ans == NULL ){
			  Node * copy = CopyNode( node ) ;
			  return copy ;
			} // if
		  } // if
		  else if( part.size() == 0 ) {
		  	error = 2 ;
		  	return head ;
		  } // else if
		  else return FuncNodes[i] ; // error != 0
		} // if
		
		if( ans == NULL ) return cond ;
	  } // else
	} // for
	
	if( ans == NULL ) {
	  FuncName = "cond", error = 5 ;
	  return head ;
	} // if
	else return ans ;
  } // if
  else {
  	error = 1 ;
  	return head ;
  } // else
} // Cond()

NodePtr FuncTable( string str, string & FuncType ) { // 已存在的func
  Node * ans = new Node ;
  ans->parent = NULL ;
  ans->left = NULL ;
  ans->right = NULL ;
  ans->type = "STRING" ;
  if( str == "\'" || str == "#<procedure quote>" || str == "quote" ){
  	FuncType = "QUOTE" ;
  	ans->name = "#<procedure quote>" ;
  	return ans ;
  } // if
  else if ( str == "cons" || str == "#<procedure cons>" ) { 
    FuncType = "CONS" ;
    ans->name = "#<procedure cons>" ;
    return ans ;
  } // if
  else if ( str == "list" || str == "#<procedure list>" ) {
    FuncType = "LIST" ;
    ans->name = "#<procedure list>" ;
    return ans ;
  } // else if

  else if ( str == "car" || str == "#<procedure car>" ) {
    FuncType = "CAR" ;
    ans->name = "#<procedure car>" ;
    return ans ;
  } // else if
  else if ( str == "cdr" || str == "#<procedure cdr>" ) {
    FuncType = "CDR" ;
    ans->name = "#<procedure cdr>" ;
    return ans ;
  } // else if
  else if ( str == "atom?" || str == "#<procedure atom?>" ) {
    FuncType = "ATOM?" ;
    ans->name = "#<procedure atom?>" ;
    return ans ;
  } // else if
  else if ( str == "pair?" || str == "#<procedure pair?>" ) {
    FuncType = "PAIR?" ;
    ans->name = "#<procedure pair?>" ;
    return ans ;
  } // else if
  else if ( str == "list?" || str == "#<procedure list?>" ) {
    FuncType = "LIST?" ;
    ans->name = "#<procedure list?>" ;
    return ans ;
  } // else if
  else if ( str == "null?" || str == "#<procedure null?>" ) {
    FuncType = "NULL?" ;
    ans->name = "#<procedure null?>" ;
    return ans ;
  } // else if
  else if ( str == "integer?" || str == "#<procedure integer?>" ) {
    FuncType = "INTEGER?" ;
    ans->name = "#<procedure integer?>" ;
    return ans ;
  } // else if
  else if ( str == "real?" || str == "#<procedure real?>" ) {
    FuncType = "REAL?" ;
    ans->name = "#<procedure real?>" ;
    return ans ;
  } // else if
  else if ( str == "number?" || str == "#<procedure number?>" ) {
    FuncType = "REAL?" ;
    ans->name = "#<procedure number?>" ;
    return ans ;
  } // else if
  else if ( str == "string?" || str == "#<procedure string?>" ) {
    FuncType = "STRING?" ;
    ans->name = "#<procedure string?>" ;
    return ans ;
  } // else if
  else if ( str == "boolean?" || str == "#<procedure boolean?>" ) {
    FuncType = "BOOLEAN?" ;
    ans->name = "#<procedure boolean?>" ;
    return ans ;
  } // else if
  else if ( str == "symbol?" || str == "#<procedure symbol?>" ) {
    FuncType = "SYMBOL?" ;
    ans->name = "#<procedure symbol?>" ;
    return ans ;
  } // else if
  else if ( str == "+" || str == "#<procedure +>" ) {
    FuncType = "ADD" ;
    ans->name = "#<procedure +>" ;
    return ans ;
  } // else if
  else if ( str == "-" || str == "#<procedure ->" ) {
    FuncType = "MINUS" ;
    ans->name = "#<procedure ->" ;
    return ans ;
  } // else if
  else if ( str == "*" || str == "#<procedure *>" ) {
    FuncType = "MULTIPLE" ;
    ans->name = "#<procedure *>" ;
    return ans ;
  } // else if
  else if ( str == "/" || str == "#<procedure />" ) {
    FuncType = "DEVISION" ;
    ans->name = "#<procedure />" ;
    return ans ;
  } // else if
  else if ( str == "not" || str == "#<procedure not>" ) {
    FuncType = "NOT" ;
    ans->name = "#<procedure not>" ;
    return ans ;
  } // else if
  else if ( str == "and" || str == "#<procedure and>" ) {
    FuncType = "AND" ;
    ans->name = "#<procedure and>" ;
    return ans ;
  } // else if
  else if ( str == "or" || str == "#<procedure or>" ) {
    FuncType = "OR" ;
    ans->name = "#<procedure or>" ;
    return ans ;
  } // else if
  else if ( str == ">" || str == "#<procedure >>" ) {
    FuncType = "BIGGER" ;
    ans->name = "#<procedure >>" ;
    return ans ;
  } // else if
  else if ( str == ">=" || str == "#<procedure >=>" ) {
    FuncType = "BIGEQU" ;
    ans->name = "#<procedure >=>" ;
    return ans ;
  } // else if
  else if ( str == "<" || str == "#<procedure <>" ) {
    FuncType = "LESS" ;
    ans->name = "#<procedure <>" ;
    return ans ;
  } // else if
  else if ( str == "<=" || str == "#<procedure <=>" ) {
    FuncType = "LESSEQU" ;
    ans->name = "#<procedure <=>" ;
    return ans ;
  } // else if
  else if ( str == "=" || str == "#<procedure =>" ) {
    FuncType = "EQUAL" ;
    ans->name = "#<procedure =>" ;
    return ans ;
  } // else if
  else if ( str == "string-append" || str == "#<procedure string-append>" ) {
    FuncType = "STRINGAPPEND" ;
    ans->name = "#<procedure string-append>" ;
    return ans ;
  } // else if
  else if ( str == "string>?" || str == "#<procedure string>?>" ) {
    FuncType = "STRINGBIGGER" ;
    ans->name = "#<procedure string>?>" ;
    return ans ;
  } // else if
  else if ( str == "string<?" || str == "#<procedure string<?>" ) {
    FuncType = "STRINGLESS" ;
    ans->name = "#<procedure string<?>" ;
    return ans ;
  } // else if
  else if ( str == "string=?" || str == "#<procedure string=?>" ) {
    FuncType = "STRINGEQU" ;
    ans->name = "#<procedure string=?>" ;
    return ans ;
  } // else if
  else if ( str == "eqv?" || str == "#<procedure eqv?>" ) {
    FuncType = "EQV?" ;
    ans->name = "#<procedure eqv?>" ;
    return ans ;
  } // else if
  else if ( str == "equal?" || str == "#<procedure equal?>" ) {
    FuncType = "EQUAL?" ;
    ans->name = "#<procedure equal?>" ;
    return ans ;
  } // else if
  else if ( str == "begin" || str == "#<procedure begin>" ) {
    FuncType = "BEGIN" ;
    ans->name = "#<procedure begin>" ;
    return ans ;
  } // else if
  else if ( str == "if" || str == "#<procedure if>" ) {
    FuncType = "IF" ;
    ans->name = "#<procedure if>" ;
    return ans ;
  } // else if
  else if ( str == "cond" || str == "#<procedure cond>" ) {
    FuncType = "COND" ;
    ans->name = "#<procedure cond>" ;
    return ans ;
  } // else if
  else if ( str == "define" || str == "#<procedure define>" ) {
    FuncType = "DEFINE" ;
    ans->name = "#<procedure define>" ;
    return ans ;
  } // else if
  else if ( str == "clean-environment" || str == "#<procedure clean-environment>" ) {
    FuncType = "CLEANENV" ;
    ans->name = "#<procedure clean-environment>" ;
    return ans ;
  } // else if
  else if ( str == "exit" || str == "#<procedure exit>" ) {
    FuncType = "EXIT" ;
    ans->name = "#<procedure exit>" ;
    return ans ;
  } // else if
  else if ( str == "let" || str == "#<procedure let>" ) {
    FuncType = "LET" ;
    ans->name = "#<procedure let>" ;
    return ans ;
  } // else if
  else if ( str == "lambda" || str == "#<procedure lambda>" ) {
    FuncType = "LAMBDA" ;
    ans->name = "#<procedure lambda>" ;
    return ans ;
  } // else if
  else if ( str == "verbose" || str == "#<procedure verbose>" ) {
    FuncType = "VERB" ;
    ans->name = "#<procedure verbose>" ;
    return ans ;
  } // else if
  else if ( str == "verbose?" || str == "#<procedure verbose?>" ) {
    FuncType = "VERB?" ;
    ans->name = "#<procedure verbose?" ;
    return ans ;
  } // else if
  else if ( FuncType == "DEFINEDFUNC" ) return ans ;
  else{
    FuncType = "OTHERS" ; 
    return NULL ;
  } // else if

} // FuncTable()

bool IsInTable( vector<Table> & table, string str, int & pos, string & FuncType ) {
  if ( table.size() != 0 ) {
    for ( pos = 0 ; pos <= table.size() - 1 ; pos++ ) {
      if ( str == table[ pos ].name ) {
      	FuncType = table[ pos ].type ;
      	return true ;
	  } // if
    } // for
  } // if
  else return false ; 
 
  pos = table.size() ;
  return false ;
} // IsinChart()

NodePtr Define( Node * head, string &FuncName, vector<NodePtr> FuncNodes, int & error, vector<Table> local_table ){
  // (define a (myCons car cdr))
  //cout << "DEFINE............................" << head->name << endl;
  //cout << "0*****************************************" << FuncNodes.size() << endl;
  string FuncType = "\0" ;
  if( head->parent == NULL ){
  	//cout << "1--------------------------" << endl;
  	if( FuncNodes.size() == 2 ) ;
  	else {
  	  error = 2 ;
  	  return head ;
	} // else
	//cout << "-------------------" << FuncNodes[0]->left->name << endl;
  	if( FuncNodes[0]->left == NULL && FuncNodes[0]->right == NULL ){ //要定義的是一個symbol
      //cout << "DEFIN-----------------" << FuncNodes.size() << endl ;
	  if( FuncNodes.size() == 2 ){ //檢查格式 
	    Node * node = FuncTable( FuncNodes[0]->name, FuncType ) ;
	    //cout << "*****************************" << FuncType << " " << FuncNodes[0]->name << " " << FuncNodes[0]->type << endl; 
	    if( FuncType == "OTHERS" ){ //是新的要定義的 
	      int pos = 0 ;
	      //cout << "1********************************************" << endl;
	      if( FuncNodes[0]->type == "SYMBOL" && IsInTable( table, FuncNodes[0]->name, pos, FuncType ) ){ //有在table裡面
		    Node * node1 = ProcessFunc( FuncNodes[1], FuncName, error, local_table, FuncType ) ;
            //cout << "......................" << node1->name << endl;
		   if( error == 0 ){
		   	  int position = 0 ;
		   	  //cout << "......................" << node1->function.size() << " " << node1->type << endl;
		      if ( node1->function.size() == 0 ) { 
		         if ( node1->type != "DEFINEDFUNC" ) {
                  table[pos].parameter1.clear() ;
                  table[pos].parameter2.clear() ;
                  table[pos].behavior.clear() ;
                  table[pos].point = node1 ; 
                  table[pos].origin = FuncNodes[1] ;   
                  table[pos].type = FuncType ;
                } // if
                else {
                  string str = node1->name ;
                  position = 0 ;
                  if ( node1->name.size() >= 12 ) {
                    str = "\0" ;
                    //cout << "------------------" << endl;
                    for ( int i = 12 ; i <= node1->name.size() - 2 ; i++ )
                        str = str + node1->name[i] ;
                    } // if
                    IsInTable( table, str, position, FuncType ) ;
                    table[pos].origin = FuncNodes[1] ;   
                    table[pos].type = "DEFINEDFUNC" ;  
                    table[pos].parameter1 = table[position].parameter1 ;
                    table[pos].parameter2 = table[position].parameter2 ;
                    table[pos].behavior = table[position].behavior ;
                    table[pos].point = node1 ; 
                    
                } // else  
              } // if  
              else { // lambda有放變數 
                position = 0 ;
                IsInTable( table, FuncNodes[1]->name, position, FuncType ) ;
		        table[pos].parameter1.clear() ;
		        table[pos].parameter2 = node1->parameters ;
		        table[pos].behavior = node1->function ;
		        table[pos].point = node1 ; 
		        table[pos].origin = FuncNodes[1] ;   
		        table[pos].type = "DEFINEDFUNC" ; 
              } // else 
                //cout << "........................" << table[pos].behavior.size() << endl;
                cout << FuncNodes[0]->name << " defined\n" ;
                return head ;
			 } // if
			  else return FuncNodes[1] ;
		    } // if
		    else if ( FuncNodes[0]->type != "SYMBOL" ) {
		     //cout << "------------------" << FuncNodes[0]->name << " "<< FuncNodes[0]->type << endl;
		   	 error = 2 ;
		   	 return head ;
		   } // if
           else { // error != 0 
             //cout << "*************************************" << endl;
             Table temp ;
             Node * tnode = ProcessFunc( FuncNodes[1], FuncName, error, local_table, FuncType ) ; 
             if ( error == 0 ) {
               if ( tnode->function.size() != 0 ) {
                 temp.name = FuncNodes[0]->name ;
                 temp.parameter1.clear() ;
                 temp.parameter2 = tnode->parameters ;
                 temp.behavior = tnode->function ;
                 temp.point = tnode ; 
                 temp.origin = FuncNodes[1] ;   
                 temp.type = "DEFINEDFUNC" ;
               } // else if  
               else {
                 if ( tnode->type != "DEFINEDFUNC" ) {
                   temp.name = FuncNodes[0]->name ;
                   temp.origin = FuncNodes[1] ;
                   temp.point = tnode ;
                   temp.type = FuncType ; 
                   temp.parameter1.clear() ;
                   temp.parameter2.clear() ;
                   temp.behavior.clear() ;
                 } // if
                 else {
                   string str = tnode->name ;
                   int tpos = 0 ;
                   if ( tnode->name.size() >= 12 ) {
                     str = "\0" ;
                     for ( int i = 12 ; i < tnode->name.size() - 1 ; i++ )
                       str = str + tnode->name[i] ;
                   } // if
          
                   IsInTable( table, str, tpos, FuncType )  ;
                   temp.name = FuncNodes[0]->name ;
                   temp.origin = FuncNodes[1] ;
                   temp.point = tnode ;
                   temp.type = "DEFINEDFUNC" ; 
                   temp.parameter1 = table[tpos].parameter1 ;
                   temp.parameter2 = table[tpos].parameter2 ;
                   temp.behavior = table[tpos].behavior ;    
                 } // else
               } // else

               table.push_back( temp ) ;
               //cout << "..................." << endl;
               cout << temp.name << " defined\n" ;
               return head ;
             } // if
             else return tnode ;
           } // else  
		 } // if
		 else {
		   error = 2 ;
		   return head ; //要定義的是原本就有的func 
	     } // else
	   } // if
	   else {
		error = 2 ;
		return head ; 
	   } // else
	} // if 
	else { //要定義的是樹 
	  //cout << "***************" << endl;
      vector<NodePtr> pNode ;
      Table temp ;
      Node * node2 = new Node ;
      node2->parent = NULL ;
      node2->left = NULL ;
      node2->right = NULL ;
      pNode.push_back(FuncNodes[0]->left) ;
      /*
      //cout << "*******************" << FuncNodes[0]->left->name << endl;
      Node * node = FuncTable( FuncNodes[0]->left->name, FuncType ) ;
      //cout << "****************" << FuncNodes[0]->name << " " << FuncType << endl;
      if( FuncType == "OTHERS" ) pNode.push_back( FuncNodes[0]->left ) ;
      else{
        error = 2;
        return head ;
	  } // else
       */
      GetContent( FuncNodes[0], pNode, error ) ; // define ( x 5 6 ) ...中的 x 5 6
      
      if ( error != 0 ) return FuncNodes[0]->left ;
      else if ( pNode[0]->left != NULL ) { //如果是葉子 
	    error = 2 ;
	    return FuncNodes[0]->left ; 
      } // else if
      
      for ( int i = 1 ; i < pNode.size() ; i++ ) { // 確認不是基本的函數且所有變數都是sybbol
        if ( pNode[i]->left == NULL && pNode[i]->right == NULL && pNode[i]->type == "SYMBOL" ) {
          FuncTable( pNode[i]->name, FuncType ) ;
          //cout << i << " " << pNode[i]->name << " " << FuncType << endl;
          if ( FuncType == "OTHERS" )  temp.parameter1.push_back( pNode[i]->name ) ;
          else{
          	error = 2 ; 
		    return head ; 
		  } // else
          //cout << ".................." << temp.parameter1.size() << endl;
        } // if
        else {
          error = 2 ;
          return head ;
        } // else
      } // for

      int pos = 0 ;
      if( IsInTable( table, pNode[0]->name, pos, FuncType ) ) { // ( x A B 中的 x 
        // 已經在table裡面了，直接改table裡面的東西 
        //cout << "-------------------------" << endl ;
        node2->name = "#<procedure " + pNode[0]->name + ">" ;
        table[pos].parameter2.clear() ;
        table[pos].behavior.clear() ;
		node2->type = "DEFINEDFUNC" ;
        table[pos].parameter1 = temp.parameter1 ; 
        for ( int i = 1 ; i < FuncNodes.size() ; i++ ) { //將要做的事情都放進去 
          table[pos].behavior.push_back(FuncNodes[i] ) ;
		} // for 
		
        table[pos].origin = FuncNodes[1] ; 
        table[pos].point = node2 ; 
        table[pos].type = "DEFINEDFUNC" ;
        //cout << "................" << endl;
        cout << FuncNodes[0]->left->name << " defined\n" ;
        return head ;
      } // if
      else { 
        //準備新增一個table1進table 
        Table temptable ;
        node2->name = "#<procedure " + pNode[0]->name + ">" ;
        node2->type = "DEFINEDFUNC" ;
        temptable.name = FuncNodes[0]->left->name ;
        temptable.origin = FuncNodes[1] ;
        temptable.point = node2 ;
        temptable.type = "DEFINEDFUNC" ;
        
        for ( int i = 1 ; i < FuncNodes.size() ; i++ ) { //將要做的事情都放進去 
          temptable.behavior.push_back( FuncNodes[i] ) ;
		} // for
          
        temptable.parameter1 = temp.parameter1 ;
        temptable.parameter2.clear() ;
        table.push_back( temptable ) ;
        cout << temptable.name << " defined\n" ;
		//cout << ".................." << pos << " " << table[pos].name << " " << table[pos].behavior.size() << endl;
        //cout << "**************" << local_table.size() << endl;
        return head ;
      } // else  

    } // else
  } // if
  else {
    error = 11 ;
	return head ; // head->parent != NULL
  } // else
} // Define()


/*test
NodePtr Define( Node * head, string & Fname, vector<NodePtr> N_list, int & err, vector<Table> local_chart ) {
                
  string func_num = "\0" ;

  if ( head->parent != NULL ) {
    err = 11 ;
    return head ;
  } // if
  else { // arg數字正確 
    if ( N_list[0]->left == NULL && N_list[0]->right == NULL ) { // 想定義的是symbol 
      if ( N_list.size() != 2 ) {
        err = 2 ;
        return head ;
      } // else if

      FuncTable( N_list[0]->name, func_num ) ;
      if ( func_num != "OTHERS" ) { // 想定義的是原始func
        err = 2 ;
        return head ;         
      }  // if

      int position = 0 ; 
      if  ( N_list[0]->type == "SYMBOL" && 
            IsInTable( table, N_list[0]->name, position, func_num ) == true ) { 
        // 已經在表裡面了   
        Node * tmp = ProcessFunc( N_list[1], Fname,  err, local_chart, func_num ) ;
        if ( err != 0 ) {
          return N_list[1] ;
        } // if
        else {
          int p_2 = 0 ; 
          if ( tmp->function.size() != 0 ) { // lambda 
            IsInTable( table, N_list[1]->name, p_2, func_num ) ;
            table[position].parameter1.clear() ;
            table[position].parameter2 = tmp->parameters ;
            table[position].behavior = tmp->function ;
            table[position].point = tmp ; 
            table[position].origin = N_list[1] ;   
            table[position].type = "OTHERS" ; 
          } // if  
          else  { 
            if ( tmp->type != "OTHERS" ) {
              table[position].parameter1.clear() ;
              table[position].parameter2.clear() ;
              table[position].behavior.clear() ;
              table[position].point = tmp ; 
              table[position].origin = N_list[1] ;   
              table[position].type = func_num ;
            } // if
            else {
              string need = tmp->name ;
              int p_2 = 0 ;
              if ( tmp->name.size() >= 12 ) {
                need = "\0" ;
                for ( int i = 12 ; i <= tmp->name.size() - 2 ; i++ )
                  need = need + tmp->name[i] ;
              } // if
          
              IsInTable( table, need, p_2, func_num )  ;
              table[position].parameter1 = table[p_2].parameter1 ;
              table[position].parameter2 = table[p_2].parameter2 ;
              table[position].behavior = table[p_2].behavior ;
              table[position].point = tmp ; 
              table[position].origin = N_list[1] ;   
              table[position].type = "OTHERS" ;  
            } // else
          } // else 

          cout << N_list[0]->name << " defined\n" ;
          return head ;
        } // else 
      } // if 
      else if ( N_list[0]->type != "SYMBOL" ) {
        err = 2 ;
        return head ;
      } // else if
      else {
        Table tmp ;
        Node * test = ProcessFunc( N_list[1], Fname,  err, local_chart, func_num ) ;
        if ( err != 0 ) {
          return test ;
        } // if
        else {
          if ( test->function.size() != 0 ) {
            tmp.name = N_list[0]->name ;
            tmp.parameter1.clear() ;
            tmp.parameter2 = test->parameters ;
            tmp.behavior = test->function ;
            tmp.point = test ; 
            tmp.origin = N_list[1] ;   
            tmp.type = "OTHERS" ;
          } // else if  
          else {
            if ( test->type != "OTHERS" ) {
              tmp.name = N_list[0]->name ;
              tmp.origin = N_list[1] ;
              tmp.point = test ;
              tmp.type = func_num ; 
              tmp.parameter1.clear() ;
              tmp.parameter2.clear() ;
              tmp.behavior.clear() ;
            } // if
            else {
              string need = test->name ;
              int p_2 = 0 ;
              if ( test->name.size() >= 12 ) {
                need = "\0" ;
                for ( int i = 12 ; i <= test->name.size() - 2 ; i++ )
                  need = need + test->name[i] ;
              } // if
          
              IsInTable( table, need, p_2, func_num )  ;
              tmp.name = N_list[0]->name ;
              tmp.origin = N_list[1] ;
              tmp.point = test ;
              tmp.type = "OTHERS" ; 
              tmp.parameter1 = table[p_2].parameter1 ;
              tmp.parameter2 = table[p_2].parameter2 ;
              tmp.behavior = table[p_2].behavior ;    
            } // else
          } // else

          table.push_back( tmp ) ;
          cout << tmp.name << " defined\n" ;
          return head ;
        } // else
      } // else  
    } // if
    else { // 變數名稱不單單只是名稱而是樹
      vector<NodePtr> para ;
      Table tmp ;
      Node * arg = new Node ;
      arg->left = NULL ;
      arg->right = NULL ;
      arg->parent = NULL ;

      para.push_back( N_list[0]->left ) ; 
      GetContent( N_list[0], para, err ) ; // ( define ( a 4 3 ) ...中的 a, 4, 3 
      if ( err != 0 ) {
        return N_list[0]->left ;
      } // if
      else if ( para[0]->left != NULL ) { //  a 要是葉子 
        err = 2 ;
        return N_list[0]->left ;
      } // else if

      for ( int i = 1 ; i <= para.size() - 1 ; i++ ) { // 確認所有變數都是sybbol且不是基本函示定義 
        if ( para[i]->left == NULL && para[i]->right == NULL && para[i]->type == "SYMBOL" ) {
          FuncTable( para[i]->name, func_num ) ;
          if ( func_num != "OTHERS" ) { // 想定義的是原始func
            err = 2 ;
            return head ;         
          }  // if 
          else 
            tmp.parameter1.push_back( para[i]->name ) ; // ( a B C )的順序跟名稱     
        } // if
        else {
          err = 2 ;
          return head ;
        } // else
      } // for

      int position = 0 ;

      if  ( IsInTable( table, para[0]->name, position, func_num ) == true ) { // ( a B X 中的 a 
        // 已經在表裡面了 
        arg->name = "#<procedure " + para[0]->name + ">" ;
        arg->type = "OTHERS" ;
        table[position].parameter1 = tmp.parameter1 ;
        table[position].parameter2.clear() ;
        table[position].behavior.clear() ;
        for ( int i = 1 ; i <= N_list.size() - 1 ; i++ ) 
          table[position].behavior.push_back( N_list[i] ) ;

        table[position].point = arg ; 
        table[position].origin = N_list[1] ;   
        table[position].type = "OTHERS" ;

        cout << N_list[0]->left->name << " defined\n" ;
        return head ;
      } // if
      else {  
        Table tmp_1 ;
        arg->name = "#<procedure " + para[0]->name + ">" ;
        arg->type = "DEFINEDFUNC" ;
        tmp_1.name = N_list[0]->left->name ;
        tmp_1.origin = N_list[1] ;
        tmp_1.point = arg ;
        tmp_1.type = "DEFINEDFUNC" ;
        for ( int i = 1 ; i <= N_list.size() - 1 ; i++ ) 
          tmp_1.behavior.push_back( N_list[i] ) ;

        tmp_1.parameter1 = tmp.parameter1 ;
        tmp_1.parameter2.clear() ;
        table.push_back( tmp_1 ) ;
        cout << tmp_1.name << " defined\n" ;
        return head ;
      } // else  

    } // else
  } // else 
} // Define()
*/

NodePtr CleanEnv( Node * head, vector<NodePtr> FuncNodes, int & error ) {
  if( head->parent == NULL ){
  	if( FuncNodes.size() == 0 ){
  	  table.clear() ;
  	  cout << "environment cleaned\n" ;
      return head ;
	} // if
	else{
	  error = 1 ;
	  return head ;
	}
  } // if
  else {
  	error = 11 ;
  	return head ;
  } // else 
} // CleanEnv()

NodePtr Exit( Node * head, string &FuncName, vector<NodePtr> FuncNodes, int & error, vector<Table> local_table ){
  if( head->parent == NULL ){
  	if( FuncNodes.size() == 0 ) return head ;
	else{
	  error = 1 ;
	  return head ;
	} // else
  } // if
  else{
  	error = 11 ;
  	return head ;
  } // else

} // Exit()

NodePtr Let( Node * head, string &FuncName, vector<NodePtr> FuncNodes, int & error, vector<Table> local_table ){
	//( let ( (x 3) (y '(1 2 3))
    //      )
    //      (cons 1 '(4 5))       ; this will be evaluated ; but no use
    //      (cons x (cdr y))      ; the value of this one is the value of LET
    //)
    
  vector<Table> temp_local_table = local_table  ; // 定義local variable

  if ( FuncNodes.size() >= 2 ) {
    if ( FuncNodes[0]->left == NULL && FuncNodes[0]->right == NULL && FuncNodes[0]->type == "NIL" ) ;
    else {
      vector<NodePtr> local_node ;
      if ( FuncNodes[0]->left == NULL && FuncNodes[0]->right == NULL ) {
        error = 2 ;
        return head ; 
      } // if
      else local_node.push_back( FuncNodes[0]->left ) ;
      
      //取區域變數 
      GetContent( FuncNodes[0], local_node, error ) ;
      //cout << "---------------" << error << " " << local_node.size() << endl;
      if( error == 0 ){
        for ( int i = 0 ; i < local_node.size() ; i++ ) { // 處理變數 
          //cout << "........................." << i << endl;
          if( local_node[i]->name == "\0" ){
          	
          	vector<NodePtr> node ; 
          	int pos = 0 ;
            string local_FuncType = "\0" ;
            Node * defined = local_node[i]->left ; //要define的區域變數
            //cout << "..........................." << endl;
            if( defined->left == NULL && defined->right == NULL ){
              FuncTable( local_node[i]->name, local_FuncType )  ;
              //cout << "-----------------" << local_FuncType << endl;
              if( local_FuncType == "OTHERS" ){ //非primitive function
                GetContent( local_node[i], node, error ) ; // ( (x 3) -> x，3 
                //cout << "---------------------" << local_node.size() << endl;
                if ( error != 0 || node.size() != 1 ) {
                  //cout << "0****************************" << endl;
                  error = 2 ;
                  return head ;
                } // if
                else {
                  if ( defined->type == "SYMBOL" && IsInTable( local_table, defined->name, pos, local_FuncType ) ) { // 已經定義過
                    //直接改在table中的資訊 
					Node * tmp = ProcessFunc( node[0], FuncName, error, temp_local_table, local_FuncType ) ;
                    if( error == 0 ){
                      local_table[pos].point = tmp ; 
                      local_table[pos].origin = node[0] ;   
                      local_table[pos].type = local_FuncType ;
					} // if
                    else return node[0] ;       
                  } // if
                  else { // 未被定義過，丟進local_table中 
                    Table tmp ;
                    Node * test = ProcessFunc( node[0], FuncName, error, temp_local_table, local_FuncType ) ;
                    if( error == 0 ){
                      tmp.name = defined->name ;
                      tmp.origin = node[0] ;
                      tmp.point = test ;
                      tmp.type = local_FuncType ; 
                      local_table.push_back( tmp ) ;
					} // if
					else return test ;
                  } // else
                } // else
			  } // if
			  else{
			  	//cout << "1****************************" << i << endl;
			    error = 2 ;
			  	return head ;
			  } // else  
			} // if
			else {
			  //cout << "2*************************" << i << endl;
			  error = 2 ;
			  return head ;
			} // else
		  } // if
		  else{
		  	//cout << "3*************************" << i << endl;
		  	error = 2 ;
		  	return head ;
		  } // else     
        } // for
	  } // if
	  else{
	  	error = 2 ;
	  	return head ;
	  } // else
    } // else
    
    string local_FuncType = "\0" ;
    Node * ans = NULL ;
    for ( int i = 1 ; i < FuncNodes.size() ; i++ ) {
      ans = ProcessFunc( FuncNodes[i], FuncName, error, local_table, local_FuncType ) ;
      if ( error != 0 ) return ans ;
    } // for 
    
    if( ans != NULL ){
      ans = CopyNode( ans ) ;
      return ans ; 
	} // if
	else{
	  error = 2 ;
      return head ;
	} // else
  } // if
  else {
    error = 2 ;
    return head ;
  } // else
} // Let()

NodePtr RunDefFunc( Node * head, string & FuncName, vector<NodePtr> FuncNodes, int & error, vector<Table> local_table, Table temptable, NodePtr LocalFunc ) {   
  //cout << "BEGIN*************************" << LocalFunc->function.size() << " " << LocalFunc->name << " " << temptable.parameter1.size() << endl ;
  string FuncType = "\0" ;
  int position = 0 ;
  //cout << LocalFunc->name << "...................." << LocalFunc->function.size() << FuncNodes.size() << endl;
  //檢查傳進來的變數數量有沒有一樣
  if( LocalFunc->function.size() == 0 ){ //沒有參數
    //cout << "0**********************" << temptable.name << " " << temptable.parameter1.size() << " " << FuncNodes.size() << endl;
  	if ( temptable.parameter1.size() == FuncNodes.size() ) ;
  	else {
      error = 1 ; 
      FuncName = temptable.name ;
    } // else
  } // if
  else{
  	//cout << "1**********************" << endl;
  	if ( LocalFunc->parameters.size() == FuncNodes.size() ) ;
  	else { 
      error = 1 ;
      FuncName = "lambda" ;
      return head ;
    } // else
    
  } // else
  
  //cout << "HALF------------------" << LocalFunc->name << " " << LocalFunc->function.size() << " " << error << endl;
  if ( LocalFunc->function.size() == 0 ) { //沒有參數 
    //cout << "2**********************" << temptable.parameter1.size() << endl;
    if ( temptable.parameter1.size() == 0 ) {  // ( define a ( lambda) )
      // 把後面lamda所要做的東西拿出來 
      Node * node = ProcessFunc( temptable.behavior[0], FuncName, error, local_table, FuncType ) ; 
      if ( error == 0 ) ;
      else return node ;

      return RunDefFunc( temptable.behavior[0], FuncName, FuncNodes, error, local_table, temptable, node ) ;
    } // if
    else {
      vector<Table> Initial_table = local_table ;
      vector<Table> parameter ; // 區域變數 
      Node * ans = NULL ;
      for ( int i = 0 ; i < temptable.parameter1.size() ; i++ ) { // 本地定義解決 
        Table temp ;
        if ( !IsInTable( parameter, temptable.parameter1[i], position, FuncType ) ) { //沒有在table裡就放入table 
          //cout << "........................." << endl;
          Node * node = ProcessFunc( FuncNodes[i], FuncName, error, Initial_table, FuncType ) ;
          if ( error == 0  ){
          	//cout << "@@@@@@@@@@@@@@@@@@@" << node->type << endl; 
          	if ( node->type == "DEFINEDFUNC" ) { //如果是被定義過的function，就設定資訊準備丟進table 
          	  //cout << "@@@@@@@@@@@@@@@@" << endl;
          	  string nodename = node->name ;
              int pos = 0 ;
              if ( node->name.size() >= 12 ) {
                nodename = "\0" ;
                for ( int i = 12 ; i <= node->name.size() - 2 ; i++ ){
                	nodename = nodename + node->name[i] ;
			    } // for
                
              } // if

              IsInTable( table, nodename, pos, FuncType )  ; 
              temp.parameter1 = table[pos].parameter1 ;
              temp.parameter2 = table[pos].parameter2 ;
              temp.behavior = table[pos].behavior ;
              temp.origin = table[pos].origin ;
            } // if
            else {
              temp.parameter1.clear() ;
              temp.parameter2.clear() ;
              temp.behavior.clear() ;
              temp.origin = FuncNodes[i] ;
            } // else
            
            temp.name = temptable.parameter1[i] ;
            temp.type = FuncType ;
            temp.point = node ;
            parameter.push_back( temp ) ;
		  } // if
          else return node ;
        } // if
        else { //如果是在table裡面的 
          Node * node = ProcessFunc( FuncNodes[i], FuncName, error, Initial_table, FuncType ) ; //看第一個要做甚麼 
          if ( error == 0  ){
          	if ( node->type == "DEFINEDFUNC" ) { //如果是被定義的function就設定資訊，要有變數動作之類的 
              string nodename = node->name ;
              int pos = 0 ;
              if ( node->name.size() >= 12 ) { 
                nodename = "\0" ;
                for ( int i = 12 ; i <= node->name.size() - 2 ; i++ )
                  nodename = nodename + node->name[0] ;
              } // if

              parameter[position].parameter1 = table[pos].parameter1 ;
              parameter[position].parameter2 = table[pos].parameter2 ;
              parameter[position].behavior = table[pos].behavior ;
              parameter[position].point = node ; 
              parameter[position].origin = FuncNodes[i] ;   
              parameter[position].type = FuncType ;
            
            } // if
            else { //單純的變數 
           	  parameter[position].parameter1.clear() ;
              parameter[position].parameter2.clear() ;
              parameter[position].behavior.clear() ;
              parameter[position].point = node ; 
              parameter[position].origin = FuncNodes[i] ;   
              parameter[position].type = FuncType ;
            
            } // else
          } // if
          else return node ;
        } // else
      } // for
      
      //cout << "*************" << temptable.behavior.size() << endl;
      for ( int i = 0 ; i < temptable.behavior.size() ; i++ ) { //去執行定義的function 
        //cout << temptable.behavior[i]->name << "-----------" << temptable.behavior.size() << endl;
        Node * node = ProcessFunc( temptable.behavior[i], FuncName, error, parameter, FuncType ) ;
        if ( error == 0 ) ;
        else return node ;
        ans = node ;
      } // for

      if ( ans != NULL ) {
        ans = CopyNode( ans ) ;
        return ans ;
      } // if
      else {
      	//cout << "HERE--------------" << endl;
      	error = 5 ;
        return head ;
        
      } // else
    } // else
  } // if
  else { // 定義(define....)
    //cout << "3**********************" << endl;
    if ( LocalFunc->parameters.size() == 0 ) { // 沒有參數的呼叫 
      vector<Table> Initial_table = local_table ;
      Node * ans = NULL ;
      for ( int i = 0 ; i < LocalFunc->function.size() ; i++ ) { //直接執行function 
        Node * RunFunc = ProcessFunc( LocalFunc->function[i], FuncName, error, Initial_table, FuncType ) ;
        if ( error == 0 ) ans = RunFunc ;
        else return RunFunc ;
      } // for

      if ( ans != NULL ) {
      	ans = CopyNode( ans ) ;
        return ans ;
      } // if
      else return head ;
    } // if 
    else { //有區域變數 
      vector<Table> parameter ; // 區域變數 
      vector<Table> Initial_table = local_table ;

      int position = 0 ; 
      for ( int i = 0 ; i < FuncNodes.size() ; i++ ) { //取變數，看是不是table中的變數 
        if ( IsInTable( parameter, LocalFunc->parameters[i], position, FuncType ) ) { // 被定義過 
          Node * node = ProcessFunc( FuncNodes[i], FuncName, error, Initial_table, FuncType ) ;
          if ( error == 0 ) {
          	parameter[position].point = node ; 
            parameter[position].origin = FuncNodes[i] ;   
            parameter[position].type = FuncType ;
            
          } // if
          else return node ;
        } // if  
        else { // 未被定義過 
          Node * node = ProcessFunc( FuncNodes[i], FuncName, error, Initial_table, FuncType ) ;
          if ( error == 0 ) { 
            Table temp ;
            temp.name =  LocalFunc->parameters[i] ;
            temp.point =  node ;
            temp.origin = FuncNodes[i] ;
            temp.type = FuncType ; 
            parameter.push_back( temp ) ; 
            
          } // if
          else return node ;
        } // else
      } // for

      Node * ans = NULL ;
      for ( int i = 0 ; i < LocalFunc->function.size() ; i++ ) { //執行function 
        //cout << "*******************" << endl;
        Node * RunFunc = ProcessFunc( LocalFunc->function[i], FuncName, error, parameter, FuncType ) ;   
        if ( error == 0 ) ans = RunFunc ;
        else return RunFunc ;
      } // for


      if ( ans != NULL ) {
        ans = CopyNode( ans ) ;
        return ans ;
      } // if
      else return head ;
    } // else 
    
  } // else
} // RunDefFunc()


NodePtr Lambda( Node * head, string & FuncName, vector<NodePtr> FuncNodes, int & error, vector<Table> local_table ) {
  Node * temp = new Node ;
  temp->parent = NULL ;
  temp->left = NULL ;
  temp->right = NULL ;   
  //cout << "LAMBDA*************************" << FuncNodes.size() << " " << FuncNodes[0]->type << endl ;
  if ( FuncNodes.size() < 2 ) { 
    error = 2 ;
    FuncName = "lambda" ; 
    return head ; 
  } // if
  else {
  	if ( FuncNodes[0]->left == NULL && FuncNodes[0]->right == NULL && FuncNodes[0]->type == "NIL" ) ;
    else if ( FuncNodes[0]->left == NULL && FuncNodes[0]->right == NULL && FuncNodes[0]->type != "NIL" ) { // 葉子只能是nil
      error = 2 ;
      FuncName = "lambda" ;
      return head ;       
    } // else if 
    else {
      //cout << "--------------------" << endl;
      vector<NodePtr> parameter ; // 參數
      Node * node = FuncNodes[0]->left ;
      node = CopyNode( node ) ;
      parameter.push_back( node ) ;
      GetContent( FuncNodes[0], parameter, error ) ; // 拿所有參數 
      if ( error == 0 ) {
        for ( int i = 0 ; i < parameter.size() ; i++ ) { 
          if ( parameter[i]->type == "SYMBOL" ) { // 確認參數都是symbol 
            string FuncType = "\0" ;
            FuncTable( parameter[i]->name, FuncType ) ; //看看是不是primitive的函數、變數 
            //cout << "---------------" << FuncType << endl;
            if ( FuncType == "OTHERS" ) temp->parameters.push_back( parameter[i]->name ) ;
            else {
              error = 2 ;
              FuncName = "lambda" ;
              return head ;
            } // else
          } // if
          else {
            error = 2 ;
            FuncName = "lambda" ;
            return head ;
          } // else
        } // for
      } // if
      else return FuncNodes[0] ; 
      
    } // else
    
    //cout << "HALF LAMBDA----------------------" << temp->function.size() << endl;
    for ( int i = 1 ; i < FuncNodes.size() ; i++ ) {
      Node * copied = FuncNodes[i] ;
      copied = CopyNode( copied ) ;
      temp->function.push_back( copied ) ;  
    } // for
    
    temp->name = "#<procedure lambda>" ;
    temp->type = "DEFINEDFUNC" ;
    //cout << "END LAMBDA-----------------------" << temp << " " << temp->name << " " << temp->function.size() << endl;
    return temp ;
  } // else
} // Lambda()


NodePtr ProcessFunc( NodePtr head, string & FuncName, int & error, vector<Table> local_table, string & FuncType ){
  Node * func ;
  FuncType = "\0" ; // FuncType->表示如define a cons的a 
  int pos = 0 ; // 在table的位置 
  bool local = false ;
  //cout << "..............." << head->name << " " << FuncName << endl;
  if ( head->left == NULL && head->right == NULL ) { // 回傳的結果 
    //cout << "1!!!!!!!!!!!!!!!!!!!!!!!!!!!!" << head->name << " " << head->type << endl;
    if ( head->type != "SYMBOL" ) {
    	//cout << "***************************"<< head->name << endl;
    	return head ;
	} // if
    else if ( IsInTable( local_table, head->name, pos, FuncType ) ){
	  //cout << "1@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@" << endl;
	  return local_table[pos].point ; // 有local以local為主
    } // else if
    else if ( IsInTable( table, head->name, pos, FuncType ) ) {
      //cout << "2@@@@@@@@@@@@@@@@@@@@@@@@@@@@@" << endl;
      return table[pos].point ;
	}
    else {
      Node * temp = FuncTable( head->name, FuncType ) ;
      if ( FuncType == "OTHERS" ) {
      	error = 10 ;
      	return head ;
	  } // else
      else return temp ;
    } // else
  } // if
  
  else if ( head->left->name == "\0" ) { // (( ....
    //cout << "2!!!!!!!!!!!!!!!!!!!!!!!!!" << head->name << endl;
    //cout << "BEGIN@@@@@@@@@@@@@@@@@@@@@" << head->name << endl ;
    func = ProcessFunc( head->left, FuncName, error, local_table, FuncType ) ;
    //cout << "END@@@@@@@@@@@@@@@@@@@@@" << func << " " << func->name << " " << func->type << " " << func->function.size() << endl; 
    FuncTable( func->name, FuncType  ) ;
    if ( func->name == "#<procedure lambda>" ) {
      pos = table.size() ;
      FuncType = "DEFINEDFUNC" ;
    } // if
  } // else if 
  
  else {
  	//cout << "3!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!" << head->name << endl; 
    if ( head->right->name != "nil" && head->left->name != "\0" && head->right->name != "\0" ) {
	  error = 7 ; 
	  return head ; // ( a . b )
    } // if
    else { 
      func = head->left ;
      //cout << "---------------------" << func->name << endl; 
      if ( IsInTable( local_table, func->name, pos, FuncType ) ) local = true, func = local_table[pos].point ; // 確認是不是使用到被定義func 
      else if ( IsInTable( table, func->name, pos, FuncType ) ) func = table[pos].point ; // 確認是不是使用到被定義func
      //
    } // else
  } // else
  
  //cout << "HALF PROCESSFUNC---------------------" << func->name << " " << FuncType << " " << func->function.size() << endl;
  if( error == 0 ){
    vector<NodePtr> FuncNodes ;
    GetContent( head, FuncNodes, error ) ;
	//cout << "BEGIN FUNCTION-----------------" << FuncNodes.size() << endl;
	if( FuncType != "DEFINEDFUNC" ) FuncTable( func->name, FuncType ) ;
	 
    if( error == 0 ) { 
      //cout << "Process----------------------------------" << func->name << endl;
      if( func->name == "cons" || FuncType == "CONS" ){
	    FuncName = "cons" ;
	    //cout << "+++++++++++++++" << endl;
		return Cons( head, FuncName, FuncNodes, error, local_table ) ;
	  } // if
      else if( func->name == "list" || FuncType == "LIST" ) {
      	FuncName = "list" ;
      	return List( head, FuncName, FuncNodes, error, local_table ) ;
	  } //else if
      else if( func->name == "quote" || func->name == "\'" || FuncType == "QUOTE" ) {
      	FuncName = "quote" ;
      	//cout << "----------------------" << endl;
      	return Quote( head ) ;
	  } // else if
      else if( func->name == "car" || FuncType =="CAR" ) {
      	FuncName = "car" ;
		return Car( head, FuncName, FuncNodes, error, local_table ) ;
	  } // else if
      else if( func->name == "cdr" || FuncType == "CDR" ) {
      	FuncName = "cdr" ;
      	return Cdr( head, FuncName, FuncNodes, error, local_table ) ;
	  } // else if
      else if( func->name == "atom?" || FuncType == "ATOM?" ) {
      	FuncName = "atom?" ;
      	return IsAtom( head, FuncName, FuncNodes, error, local_table ) ;
	  } // else if
      else if( func->name == "pair?" || FuncType == "PAIR?" ) {
      	FuncName = "pair?" ;
      	return IsPair( head, FuncName, FuncNodes, error, local_table ) ;
	  } // else if
      else if( func->name == "list?" || FuncType == "LIST?" ) {
      	FuncName = "list?" ;
      	return IsList( head, FuncName, FuncNodes, error, local_table ) ;
	  } // else if
	  else if( func->name == "null?" || FuncType == "NULL?" ) {
	  	FuncName = "null?" ;
	  	return IsNull( head, FuncName, FuncNodes, error, local_table ) ;
	  } // else if
	  else if( func->name == "integer?" || FuncType == "INTEGER?" ) {
	  	FuncName = "integer?" ;
	  	return IsInt( head, FuncName, FuncNodes, error, local_table ) ;
	  } // else if
	  else if( func->name == "real?" || FuncType == "REAL?" ) {
	  	FuncName = "real?" ;
	  	return IsReal( head, FuncName, FuncNodes, error, local_table ) ;
	  } // else if
	  else if( func->name == "number?" || FuncType == "REAL?" ) {
	  	FuncName = "number?" ;
	  	return IsNum( head, FuncName, FuncNodes, error, local_table ) ;
	  } // else if
	  else if( func->name == "string?" || FuncType == "STRING?" ) {
	  	FuncName = "string?" ;
	  	return IsString( head, FuncName, FuncNodes, error, local_table ) ;
	  } // else if
	  else if( func->name == "boolean?" || FuncType == "BOOLEAN?" ) {
	  	FuncName = "boolean?"  ;
	  	return IsBool( head, FuncName, FuncNodes, error, local_table ) ;
	  } // else if
	  else if( func->name == "symbol?" || FuncType == "SYMBOL?" ) {
	  	FuncName = "symbol?" ;
	  	return IsSymbol( head, FuncName, FuncNodes, error, local_table ) ;
	  } // else if
	  else if( func->name == "+" || FuncType == "ADD" ) {
	  	FuncName = "+" ;
	  	return Add( head, FuncName, FuncNodes, error, local_table ) ;
	  } // else if
	  else if( func->name == "-" || FuncType == "MINUS" ) {
	  	FuncName = "-" ;
	  	return Minus( head, FuncName, FuncNodes, error, local_table ) ;
	  } // else if
	  else if( func->name == "*" || FuncType == "MULTIPLE" ) {
	  	FuncName = "*" ;
	  	return Multiple( head, FuncName, FuncNodes, error, local_table ) ;
	  } // else if
	  else if( func->name == "/" || FuncType == "DEVISION" ) {
	  	FuncName = "/" ;
	  	return Division( head, FuncName, FuncNodes, error, local_table ) ;
	  } // else if
	  else if( func->name == "not" || FuncType == "NOT" ) {
	  	FuncName = "not" ;
	  	return NOT( head, FuncName, FuncNodes, error, local_table ) ;
	  } // else if
	  else if( func->name == "and" || FuncType == "AND" ) {
	  	FuncName = "and" ;
	  	return AND( head, FuncName, FuncNodes, error, local_table ) ;
	  } // else if
	  else if( func->name == "or" || FuncType == "OR" ) {
	  	FuncName = "or" ;
	  	return OR( head, FuncName, FuncNodes, error, local_table ) ;
	  } // else if
	  else if( func->name == ">" || FuncType == "BIGGER" ) {
	  	FuncName = ">" ;
	  	Node * test = Bigger( head, FuncName, FuncNodes, error, local_table ) ;
	  	//cout << "RETURNED---------------" << test << " " << test->name << " " << test->type << endl ;
	  	return test ;
	  } // else if
	  else if( func->name == ">=" || FuncType == "BIGEQU" ) {
	    FuncName = ">=" ;
	    return BiggerEqual( head, FuncName, FuncNodes, error, local_table ) ;
	  } // else if
	  else if( func->name == "<" || FuncType == "LESS" ) {
	  	FuncName = "<" ;
	  	return Less( head, FuncName, FuncNodes, error, local_table ) ;
	  } // else if
	  else if( func->name == "<=" || FuncType == "LESSEQU" ) {
	  	FuncName = "<=" ;
	  	return LessEqual( head, FuncName, FuncNodes, error, local_table ) ;
	  } // else if
	  else if( func->name == "=" || FuncType == "EQUAL" ) {
	  	FuncName = "=" ;
	  	return Equal( head, FuncName, FuncNodes, error, local_table ) ;
	  } // else if
	  else if( func->name == "string-append" || FuncType == "STRINGAPPEND" ) {
	  	FuncName = "string-append" ;
	  	Node * test = StringAppend( head, FuncName, FuncNodes, error, local_table ) ;
	  	//if( test->left != NULL ) cout << "1++++++++++++++++++++++++" << endl;
	  	//if( test->right != NULL ) cout << "2+++++++++++++++++++++++++" << endl;
	  	return test ;
	  } // else if
	  else if( func->name == "string>?" || FuncType == "STRINGBIGGER" ) {
	  	FuncName = "string>?" ;
	  	return StringBigger( head, FuncName, FuncNodes, error, local_table ) ;
	  } // else if
	  else if( func->name == "string<?" || FuncType == "STRINGLESS" ) {
	  	FuncName = "string<?" ;
	  	return StringLess( head, FuncName, FuncNodes, error, local_table ) ;
	  } // else if
	  else if( func->name == "string=?" || FuncType == "STRINGEQU" ) {
	  	FuncName = "string=?" ;
	  	return StringEqual( head, FuncName, FuncNodes, error, local_table ) ;
	  } // else if
	  else if( func->name == "eqv?" || FuncType == "EQV?" ) {
	  	FuncName = "eqv?" ;
	  	return IsEqv( head, FuncName, FuncNodes, error, local_table ) ;
	  } // else if
	  else if( func->name == "equal?" || FuncType == "EQUAL?" ) {
	  	FuncName = "equal?" ;
	  	return IsEqual( head, FuncName, FuncNodes, error, local_table ) ;
	  } // else if
	  else if( func->name == "begin" || FuncType == "BEGIN" ) {
	  	FuncName = "begin" ;
	  	return Begin( head, FuncName, FuncNodes, error, local_table ) ;
	  } // else if
	  else if( func->name == "if" || FuncType == "IF" ) {
	  	FuncName = "if" ;
	  	return If( head, FuncName, FuncNodes, error, local_table ) ;
	  } // else if
	  else if( func->name == "cond" || FuncType == "COND" ) {
	  	FuncName = "cond" ;
	  	return Cond( head, FuncName, FuncNodes, error, local_table ) ;
	  } // else if
	  else if( func->name == "define" || FuncType == "DEFINE" )  {
	  	FuncName = "define" ;
	  	//cout << "**********************************" << endl; 
	  	return Define( head, FuncName, FuncNodes, error, local_table ) ;
	  } // else if
	  else if( func->name == "clean-environment" || FuncType == "CLEANENV" ){
	  	FuncName = "clean-environment" ;
	  	return CleanEnv( head, FuncNodes, error ) ;
	  } // else if
	  else if ( func->name == "exit" || FuncType == "EXIT" ) {
        FuncName = "exit" ;
        return Exit( head, FuncName, FuncNodes, error, local_table ) ;
      } // else if
	  
      else if ( func->name == "let" || FuncType == "LET" ) {
        FuncName = "let" ;
        return Let( head, FuncName, FuncNodes, error, local_table ) ;
      } // else if
      
      else if ( FuncType == "DEFINEDFUNC" ) {
      	//cout << "*********************" << func->name << endl;
      	if( pos != table.size() ){
      	  //cout << "+++++++++++++++++++++" << endl;
      	  if ( !local ) {
      	  	//cout << "+++++++++++++" << pos << " " << table[pos].name << endl;
      	  	return RunDefFunc( head, FuncName, FuncNodes, error, local_table, table[pos], func ) ; 
		  } // if                       
          else{
          	//cout << "+++++++++++++++++" << pos << " " << local_table[pos].name << endl;
          	return RunDefFunc( head, FuncName, FuncNodes, error, local_table, local_table[pos], func ) ; 
		  } 
		} // if
		else{
		  //cout << "---------------------" << endl;
		  Table temptable ;
          temptable.point = NULL ;
          return RunDefFunc( head, FuncName, FuncNodes, error, local_table, temptable, func ) ;
		} // else
		//cout << "END DEFINEDFUNC-------------------" << endl;
      } // else if
      else if ( func->name == "lambda" || FuncType == "LAMBDA" ){
      	//cout << "HERE*****************" << endl;
      	FuncName = "lambda" ;
      	Node * check = Lambda( head, FuncName, FuncNodes, error, local_table ) ;
      	//cout << "、、、、、、、、、、" << check << " " << check->name  << " " << check->function.size() << endl;
      	return check ;
	  } 
      else if ( func->type == "INT" || func->type == "FLOAT" ) {
      	error = 8 ;
      	return func ;
	  } // else if
	  else {
        if ( func->name == "\0" ){ // 要node結果是tree 
          error = 8 ;
          return func ;
		} // if 
        else if ( IsInTable( local_table, func->name, pos, FuncType ) ){
          error = 8 ;
          return func ;
		} // if
        else if ( IsInTable( table, func->name, pos, FuncType ) ){
          error = 8 ;
          return func ;
		} // if
        else {
          error = 10 ;
          return func ;
		} // if
      } // else 
      
      //cout << "END OF FUNCTION-------------------" << endl;
	} // if 
    else return head ;
  } // if 
  else return func ;
} // ProcessFunc

bool IsExitOver( Node * head, vector<Token> FuncNodes ) { // 是不是用exit的方式over 
  if ( FuncNodes.size() != 1 && head->left->name == "exit" && head->right->name == "nil" ) return true ;
  else return false ;
} // IsExitOver()

void PrintStr( string  str )  { // 因應string的印方式 
  string ans = "\0" ;
  for ( int i = 0 ; i < str.size() ; i++ ) {
    if ( ans == "\0" && str[i] == '\\' ) ans = str[i] ;  
    else if ( ans[0] == '\\'  ) {
      if ( str[i] == '\\' ) cout << '\\'  ;
      else if ( str[i] == 'n' || str[i] == '\"' || str[i] == 't' ) {
        if ( str[i] == 'n' ) cout << endl ;  
        else if ( str[i] == '\"' ) cout << '\"' ;
        else if ( str[i] == 't'   ) cout << "\t" ;
        ans = "\0" ; 
      } // else if 
      else cout << ans << str[i] ;
      ans = "\0" ;
    } // else if
    else cout << str[i] ;
  } // for
  
  cout << endl ;
  
} // PrintStr()

void PrettyPrint( Node * head, int & space, int & num, bool & changeL ) { // 空格 , ( 後從新排版 
  //cout << "PRETTYPRINT--------------" << endl;
  if ( head == NULL ) ; //cout << "1-----------------" << endl ;
  
  else if ( head->left != NULL && head->left->name == "\0" ) {
  	//cout << "2-----------" << endl;
    for ( int i = num ; i <= space ; i++ ) {
      if ( changeL ) changeL = false ;
      else cout << " " ;
    } // for
    
    changeL = true ;
    cout << "( " ;
    space = space + 2 ;
    num = space ;
    PrettyPrint( head->left, space, num, changeL ) ;
    PrettyPrint( head->right, space, num, changeL ) ;
  } // else if
  
  else if ( head->left == NULL && head->right == NULL ) {
  	//cout << "3------------" << endl;
    if ( head->parent != NULL && head == head->parent->right ) {
      if( head->name != "nil" ){
        for( int i = num ; i <= space ; i++ ) {
          if ( changeL ) changeL = false ;
          else cout << " " ;
        } // for
 
        cout << "." << endl ;
        num = 1 ; 
        for ( int i = num ; i <= space ; i++ ) {
          if ( changeL ) changeL = false ;
          else cout << " " ;
        } // for
 
        if ( head->type != "STRING" ) cout << head->name << endl ; 
        else PrintStr( head->name ) ;
        num = 1 ; 
      } // if

      space = space - 2 ;
      for ( int i = num ; i <= space ; i++ ) {
        if ( changeL ) changeL = false ;
        else cout << " " ;
      } // for

      cout << ")" << endl ;
      num = 1 ; 
    } // if
    else if ( head->parent != NULL && head == head->parent->left ) {
    
      for ( int i = num ; i <= space ; i++ ) {
        if ( changeL ) changeL = false ;
        else cout << " " ;
      } // for
   
      if ( head->type != "STRING" ) cout << head->name << endl ;  
      else PrintStr( head->name ) ; 
      num = 1 ; 
    } // else if

  } // else if
  
  else {
  	//cout << "4----------" << endl;
    PrettyPrint( head->left, space, num, changeL ) ;
    PrettyPrint( head->right, space, num, changeL ) ; 
  } // else
 
} // PrettyPrint() 

void SmalltoBIg( string & FuncName ) { // 小寫至大寫
  for ( int i = 0 ; i < FuncName.size() ; i++ ) { // 轉成大寫迴圈
    if ( FuncName[i] >= 'a' && FuncName[i] <= 'z' ) FuncName[i] = FuncName[i] - 32 ;  
  } // for
} // Small_to_BIg()

void PrintErr_func( Node * head, int error, string FuncName ) {  
  if ( error == 1 ) cout << "ERROR (incorrect number of arguments) : " ;
  else if ( error == 2 ) { 
    SmalltoBIg(  FuncName ) ; 
    cout << "ERROR (" << FuncName << " format) : " ;
  } // else if
  else if ( error == 3 )  cout << "ERROR (" << FuncName << " with incorrect argument type) : " ;
  else if ( error == 4 ) cout << "ERROR (division by zero) : /\n" ; 
  else if ( error == 5 ) cout << "ERROR (no return value) : " ; 
  else if ( error == 6 ) cout << "ERROR (attempt to apply non-function) : " ;  
  else if ( error == 7 ) cout << "ERROR (non-list) : " ;
  else if ( error == 8 ) cout << "ERROR (attempt to apply non-function) : " ;  
  else if ( error == 10 ) cout << "ERROR (unbound symbol) : "  ;  
  else if ( error == 11 ) {
    SmalltoBIg(  FuncName ) ; // FuncName變大寫 
    cout << "ERROR (level of " << FuncName << ")" ; 
  } // else if

  if ( error == 1 ) cout << FuncName << "\n" ;
  else if ( error == 11 || error == 9 ) cout << "\n" ;
  else if ( error != 4 && error != 9 ) {
    if ( head->name != "\0" && head->left == NULL && head->right == NULL ) cout << head->name << "\n" ;
    else {
      int space = 2, num = 2 ; 
      bool changeL = true ;

      cout << "( " ; 
      PrettyPrint( head, space, num, changeL ) ; // 印樹出來 
    } // else
  } // else if
} // PrintErr_func()

void PrintErr_token( vector<Token> & node, int error, int line, int column ) { //node的錯誤 
  int pos = node.size() - 1 ;

  if ( error == 1 ) {
    cout << "ERROR (unexpected node) : atom or '(' expected when node at Line " << node[pos].line ;
    cout << " Column " << node[pos].start << " is >>" << node[pos].name << "<<" << endl ; 
  } // if
  else if ( error == 2 ) {
    cout << "ERROR (unexpected node) : ')' expected when node at Line " << node[pos].line  ;
    cout << " Column " << node[pos].start << " is >>" << node[pos].name << "<<" << endl ; 
  } // else if
  else if ( error == 3 ) {
    cout << "ERROR (no closing quote) : END-OF-LINE encountered at Line " << line << " Column " ;
    cout << column << endl ;
  } // else if
  else if ( error == 4 ) cout << "ERROR (no more input) : END-OF-FILE encountered" ; 
    
} // PrintErr_token()

int main() {
  int error = 0, line = 1, column = 1 ; // error->錯誤代碼, line->行數, column->第幾個字元
  int count = 0, ErrorFunc = 0 ; // count->建樹計oint數器 , ErrorFunc->function版錯誤代碼,FuncType->func代號 
  bool eof = false, dot = false, ChangeLine = false ; // eof->eof是否出現, dot->建樹時是否遇到點
  bool exit = false, Newline = false ; // exit->是否是用EXIT結束  
  NodePtr head = NULL, parent = NULL ; // head->每棵樹的開頭, parent->每個子結點的父子樹位置 
  vector<Token> workspace ; // list->tokenlist
  vector<Table> local_table ; // 定義表 
  string FuncName = "\0",  FuncType = "\0" ; // FuncName->func名稱 
  string nextToken = "\0" ;
  cout << "Welcome to OurScheme!" ;
  cout << "\n" ;
  
  char temp = '\0' ;
  while (  temp != '\n' ) {
    // 開頭第一行 
    scanf( "%c", &temp ) ;
  } // while
   
  while ( !eof && !exit ) { // 不斷執行動作直到輸入exit或是eof 
    cout << "\n> " ;
    // 一個字一個字讀，直到有符合的句子或不符合文法 
    
    //cout << "BEGING READ SEXP-------------------------" << endl;
    ReadSExp( workspace, error, eof, line, column, nextToken, ChangeLine, Newline ) ;
    //cout << "END READ SEXP----------------------------" << error << endl; 
    /*
    for( int i = 0  ; i < workspace.size() ; i++ ){
    	cout << workspace[i].name << " " << workspace[i].type << workspace[i].start << endl;
	}
    */
    //cout << "ERROR----------------------" << error << endl ; 
	if ( error == 0 ) { // 沒有錯誤
    // 考慮直接eof的問題 
      vector<LP> lpinfo ;
      //cout << "BUILD TREE------------------------------------" << endl;
      BuildTree( workspace, head, parent, count, dot, lpinfo ) ; // 建樹
      //cout << head->right->left->name << endl;
      //cout << "END BUILD TREE----------------------------" << endl ;
      exit = IsExitOver( head, workspace ) ; // 是否結束  
                
      if ( !exit ) {
	    //cout << "NOT EXIT-------------------------------" << endl; 
	    bool pchange = true ; // 是否換行
        int space = 2, printIndex = 2 ; // space空格數, printIndex->現在印的位置 
        //PrettyPrint( head, space, printIndex, pchange ) ;
        //cout << workspace.size() << endl;
        if ( workspace.size() == 1 ) {
          int tposition = 0 ; // table的位置 
          //cout << "----------------" << endl;
		  //cout << workspace[0].name << " " << workspace[0].type << endl ;
          if ( workspace[0].type != "SYMBOL" || IsInTable( table, workspace[0].name, tposition, FuncType ) ) {
          	if( workspace[0].type == "FLOAT" ){
          	  float num = atof( workspace[0].name.c_str() ) ;
          	  char temp[20] ;
          	  string nothing ;
          	  ftoa( num, 3, temp, nothing ) ;
          	  cout << temp << endl ;
			} // if
            else if ( workspace[0].type != "STRING" && workspace[0].type != "SYMBOL" ) 
              cout << workspace[0].name << "\n" ;
            else if ( workspace[0].type == "STRING" ) 
              PrintStr( workspace[0].name  ) ;            
            else if ( table[tposition].point->left == NULL && table[tposition].point->right == NULL )
              PrintStr( table[tposition].point->name  ) ;
            else { 
              cout << "( " ; 
              PrettyPrint( table[tposition].point, space, printIndex, pchange ) ; // 印樹出來
            } // else 
          } // if
          else { //就是定義的變數 
            Node * node = FuncTable( workspace[0].name, FuncType ) ;
            
            if ( FuncType == "OTHERS" ) PrintErr_func( head->left, 10, workspace[0].name ) ;
            else cout << node->name << "\n" ; 
              
          } // else 
        }  // if
        else { 
          //cout << "MAIN BEFORE PROCESS-----------------" << head->name << endl; 
          Node * node = ProcessFunc( head, FuncName, ErrorFunc, local_table, FuncType ) ;
          //cout << "MAIN END PROCESS-----------------" << ErrorFunc << endl ;
          /*
		  for( int i = 0  ; i < table.size() ; i++ ){
          	cout << table[i].name << endl;
		  } //
		  */
          //if( node == NULL ) cout << "!!!!!!!!!!!!!!!!!!!" << endl;
          int tposition = 0 ; // 表的位置
          string str = "\0" ;
          IsInTable( table, head->left->name, tposition, FuncType ) ;
          
          if ( table.size() != 0 &&  tposition < table.size() ) str = table[tposition].point->name ;
            
          //cout << "MAIN----------------------" << str << " " << table.size() << " " << tposition << endl;
          if ( head->left != NULL && ( head->left->name == "clean-environment" || str == "#<procedure clean-environment>" ) && ErrorFunc == 0 ) 
		    FuncName = "\0" ;
          else if ( head->left != NULL && ( head->left->name == "define" || str == "#<procedure define>" ) && ErrorFunc == 0 ) 
		    FuncName = "\0" ; // 為了define列印用 
          else if ( head->left != NULL && ( head->left->name == "exit" || str == "#<procedure exit>" ) && ErrorFunc == 0 ) {  
            exit = true ;
            FuncName = "\0" ;
          } // else if 
          else if ( ErrorFunc == 0 ) {
          	//cout << "**********************" << endl;
            if ( node->left == NULL && node->right == NULL ) {	
              if ( node->type == "STRING" ) PrintStr( node->name ) ;
              else cout << node->name << "\n" ;
            } // if
            else {
              cout << "( " ; 
              //cout << "+++++++++++++++++" << endl;
              PrettyPrint( node, space, printIndex, pchange ) ; // 印樹出來
            } // else
          } // else if
          else PrintErr_func( node, ErrorFunc, FuncName ) ;
          
          FuncName = "\0" ; 
          ErrorFunc = 0 ;
        } // else 
        
      } // if
      
      if ( eof && !exit ) PrintErr_token( workspace, 4, line, column )  ;  // 最後有eof 
      else Newline = true ; // 代表此行有回傳過東西  
    } // if
    else {
      PrintErr_token( workspace, error, line, column ) ; // 印出錯誤代碼 
      if ( eof && error != 4 ) PrintErr_token( workspace, 4, line, column )  ;
      
    } // else
    
    // 初始化專區 
    head = NULL ;
    parent = NULL ;
    count = 0 ;
    dot = false ;
    if ( error == 3 || ( error != 0 && ChangeLine ) ) error = 0 ;
    //cout << "INITIAL-------------------" << error << endl;
    //error = 0 ;
    ChangeLine = false ;
    line = 1 ;
    column = 1 ;
    // PROJ 2 初始化專區
    FuncType = "\0" ; 
    workspace.clear() ;
	  
  } // while 
 
  cout << "\n" ;
  cout << "Thanks for using OurScheme!" ;
} // main()
