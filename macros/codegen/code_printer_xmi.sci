// Copyright 2013-2015 Enpc
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
//
// Authors: J.-Ph. Chancelier (Cermics/ENPC)

function [txtcode,txtdec]=code_printer_xmi(code,declarations, keeprefs=%f)
// 

  function ty=Ctypeof(dt)
    ee=["int8","uint8","int16","uint16","int32","uint32","double","complex","boolean","b"]
    ef=["int8_t","uint8_t","int16_t","uint16_t","int32_t","uint32_t","double","double","gboolean","gboolean"]
    k=find(dt==ee(1,:))
    if isempty(k) then error('Unsupported data type: '+dt);end
    ty=ef(k)
  endfunction;
  
  function res = is_function_argument(name)
    if exists('Lres2','callers') then 
      for i=1:length(Lres2); 
	oname=getvarname(Lres2(i));
	if oname == name then 
	  res=%t; return
	end
      end
    end
    if exists('zins','callers') then 
      for i=1:length(zins); 
	oname=getvarname(zins(i));
	if oname == name then 
	  res=%t; return
	end
      end
    end
    res=%f;
  endfunction

  function xmicode = print_expr(rhs,etype="etype")
  // print an expression 
    close="";
    global _defs;
    global(Refs=hash(10)); 
    tps=["double","complex","_int8","_uint8","_int16","_uint16",...
	 "_int32","_uint32","boolean","b","int8","uint8","int16",...
	 "uint16","int32","uint32"];
    opt = type(rhs,'short');
    if opt == 'h' then opt = rhs.type; end
    if or(opt==tps) then
      // a literal value: string(valueof(rhs))
      [open,close]=codemodel_annotations(msg=string(valueof(rhs)),id=getunique(1));
      xmicode = [open;close];
      pause Tobedone
    elseif or(opt == ["m","i","b"]) then
      // nsp matrices of size 1x1; here rhs is not always a bvar 
      [open,close]=codemodel_ScalarExpression(valueof(rhs),etype=etype,id=getunique(1))
      xmicode=[open;close];
    elseif opt=="bvar" then
      // rhs is a bvar (of size 1x1 ?)
      if rhs.is_symbolic[] then
	index=[];if  is_function_argument(rhs.get_varname[]) then  index = 0;end
	[open,close]=codemodel_VariableExpression(etype=etype,variable=rhs.get_varname[],index=index);
      else
	[open,close]=codemodel_ScalarExpression(rhs.get_value[],etype=etype,id=getunique(1));
      end
      xmicode = [open;close]; 
      //-------------------------------------------------------
    elseif opt=="op" then
      // 
      t=rhs.exp
      if t(1)=="b_extract" then
	//-- binary extract 
	u=t(2)(1);i=t(2)(2);j=t(2)(3)
	// r=sprintf("(%s)",pcode_extract3(u,i,j,delims=delims)); 
	[open1,close1]=codemodel_Expression(etype=etype,type="ParenthesisExpression",id=getunique(1));
	[xmicode]=pcode_extract3(u,i,j,delims=delims,tag="expression");
	xmicode=[open1;"  "+xmicode;close1];
      elseif t(1)=="u_extract" then
	//-- unary extract 
	u=t(2)(1);i=t(2)(2);
	[open1,close1]=codemodel_Expression(etype=etype,type="ParenthesisExpression",id=getunique(1));
	[xmicode]=pcode_extract2(u,i,delims=delims,tag="expression");
	xmicode=[open1;"  "+xmicode;close1];
      elseif or(t(1)==["==","<=",">=","<>","<",">","/"]) then
	//-- operators
	ops = ["EQ","LE","GE","NE","LT","GT","DIV"]
	I= find(t(1)==["==","<=",">=","<>","<",">","/"]);
	operator=sprintf("%s_OPERATOR",ops(I));
	in1=t(2)(1);in2=t(2)(2);
	// r=print_expr(in1)+t(1)+print_expr(in2); r="("+r+")" 
	code1=print_expr(in1,etype="leftArgument");
	code2=print_expr(in2,etype="rightArgument");
	[open2,close2]=codemodel_BinaryExpression(etype="expression",operator=operator,id=getunique(1));
	[open1,close1]=codemodel_Expression(etype=etype,type="ParenthesisExpression",id=getunique(1));
	xmicode=[open1;"  "+open2;"    "+code1;"    "+code2;"  "+close2;close1];
      elseif or(t(1)==["+"]) then
	//-- + operator 
	in1=t(2)(1);in2=t(2)(2);
	if datatype(in1)=="double" | t(3).equal["overflow"] then
	  // r=print_expr(in1)+t(1)+print_expr(in2); r="("+r+")"
	  code1=print_expr(in1,etype="leftArgument");
	  code2=print_expr(in2,etype="rightArgument");
	  [open2,close2]=codemodel_BinaryExpression(etype="expression",operator="ADD_OPERATOR",id=getunique(1));
	  [open1,close1]=codemodel_Expression(etype=etype,type="ParenthesisExpression",id=getunique(1));
	  xmicode=[open1;"  "+open2;"    "+code1;"    "+code2;"  "+close2;close1];
	else
	  // specialized additions 
	  pause to_be_done
	  r="add"+datatype(in1)+"_"+t(3)+"("+print_expr(in1)+","+print_expr(in2)+")";
	  _defs=unique([_defs;"add"+datatype(in1)+"_"+t(3)])
	end
      elseif or(t(1)==["*"]) then
	// * operator 
	in1=t(2)(1);in2=t(2)(2);
	if datatype(in1)=="double" | ~t(3).equal["satur"] then
	  // r=print_expr(in1)+t(1)+print_expr(in2); r="("+r+")"
	  code1=print_expr(in1,etype="leftArgument");
	  code2=print_expr(in2,etype="rightArgument");
	  [open2,close2]=codemodel_BinaryExpression(etype="expression",operator="MUL_OPERATOR",id=getunique(1));
	  [open1,close1]=codemodel_Expression(etype=etype,type="ParenthesisExpression",id=getunique(1));
	  xmicode=[open1;"  "+open2;"    "+code1;"    "+code2;"  "+close2;close1];
	else
	  // special case of * where we call
	  fun=sprintf("mult_%s_%s",datatype(in1),t(3));
	  rest = sprintf('name=""%s"" standardFunction=""NONE""',fun);
	  global(Refs=hash(10));  // updates the references to fun
	  id=getunique(1);
	  Refs(fun)= Refs.find[fun,def='']+id+' ';
	  [open1,close1]=codemodel_Expression(etype=etype,type="CallExpression",id=id,rest=rest)
	  xmiarg1=print_expr(in1,etype="arguments");
	  xmiarg2=print_expr(in2,etype="arguments");
	  ret_type=codemodel_type(valueof(in1),etype="returnDataType");
	  xmicode=[open1;"  "+xmiarg1;"  "+xmiarg2;ret_type;close1];
	  _defs=unique([_defs;fun]);
	end
      elseif t(1)=="-" then
	// - unary and binary 
	ins=t(2);
	if length(ins)==1 then
	  in1=ins(1)
	  if datatype(in1)=="double" | ~t(3).equal["satur"] then
	    // r="-"+print_expr(in1); r="("+r+")"
	    code1=print_expr(in1,etype="argument");
	    [open2,close2]=codemodel_UnaryExpression(etype="expression",operator="UNARY_MINUS_OPERATOR",id=getunique(1));
	    [open1,close1]=codemodel_Expression(etype=etype,type="ParenthesisExpression",id=getunique(1));
	    xmicode=[open1;"  "+open2;"    "+code1;"  "+close2;close1];
	  else
	    pause to_be_done
	    r="neg"+datatype(in1)+"_"+t(3)+"("+print_expr(in1)+","+print_expr(in2)+")";r="("+r+")"
	    _defs=unique([_defs;"neg"+datatype(in1)+"_"+t(3)])
	  end
	else
	  in1=ins(1); in2=ins(2);     
	  // r=print_expr(in1)+"-"+print_expr(in2); r="("+r+")" 
	  code1=print_expr(in1,etype="leftArgument");
	  code2=print_expr(in2,etype="rightArgument");
	  [open2,close2]=codemodel_BinaryExpression(etype="expression",operator="SUB_OPERATOR",id=getunique(1));
	  [open1,close1]=codemodel_Expression(etype=etype,type="ParenthesisExpression",id=getunique(1));
	  xmicode=[open1;"  "+open2;"    "+code1;"    "+code2;"  "+close2;close1];
	end
      elseif or(t(1)==["sin","cos","tan","asin","acos","atan","sinh","cosh","tanh","sqrt","log","exp","floor","ceil"]) then
	//-- std functions 
	in1=t(2)(1);
	fun=t(1);
	if fun=="atan" && length(t(2))==2 then fun="atan2";end 
	rest = sprintf('name=""%s"" standardFunction=""%s_FUN""',fun, toupper(fun));
	[open1,close1]=codemodel_Expression(etype=etype,type="CallExpression",id=getunique(1),rest=rest)
	xmiarg=print_expr(in1,etype="arguments");
	if fun == "atan2" then 
	  xmiarg=[xmiarg;print_expr(t(2)(2),etype="arguments")];
	end
	xmicode=[open1;"  "+xmiarg;close1];
      elseif or(t(1)==["int8","uint8","int16","uint16","int32","uint32"]) then
	//-- convert to int type givent by t(1) 
	in1=t(2)(1);typ = t(2)(2).get_value[]; r= print_expr(in1);
	// XXX: should take care here of overflow  in t(3)
	code1=print_expr(in1,etype="argument");
      	select typ 
	 case "double" then 1;
	 case {"int8","uint8","int16","uint16","int32","uint32"} then 1;
	 case "b" then 1;
	else
	  printf("wrong datatype for convert\n");pause ;
	end
	I=find(t(1)==["int8","uint8","int16","uint16","int32","uint32"]);
	nbits = [8,8,16,16,32,32]; nbits=nbits(I);
	signed= ["true","false","true","false","true","false"];signed=signed(I);
	rest= sprintf( "nBits=""%d"" signed=""%s"" ",nbits,signed);
	[open_t,close_t]=codemodel_dataType(etype="targetDataType",type="TRealInteger",id=getunique(1), rest = rest);
	[open_e,close_e]=codemodel_Expression(etype=etype,type="UnaryExpression",id=getunique(1), rest ="operator=""CAST_OPERATOR""");
	xmicode=[open_e;code1;open_t;close_t;close_e]
      elseif or(t(1)==["boolean","b"]) then
	//-- convert to boolean from typ
	in1 = t(2)(1); typ = t(2)(2).get_value[];
	code1=print_expr(in1,etype="argument");
	select typ 
	 case "double" then 
	  [open_t,close_t]=codemodel_dataType(etype="targetDataType",type="TBoolean",id=getunique(1), rest = "");
	  [open_e,close_e]=codemodel_Expression(etype=etype,type="UnaryExpression",id=getunique(1), rest ="operator=""CAST_OPERATOR""");
	  xmicode=[open_e;code1;open_t;close_t;close_e]
	 case {"int8","uint8","int16","uint16","int32","uint32"} then 
	  [open_t,close_t]=codemodel_dataType(etype="targetDataType",type="TBoolean",id=getunique(1), rest = "");
	  [open_e,close_e]=codemodel_Expression(etype=etype,type="UnaryExpression",id=getunique(1), rest ="operator=""CAST_OPERATOR""");
	  xmicode=[open_e;code1;open_t;close_t;close_e]
	 case "b" then 
	  [open_t,close_t]=codemodel_dataType(etype="targetDataType",type="TBoolean",id=getunique(1), rest = "");
	  [open_e,close_e]=codemodel_Expression(etype=etype,type="UnaryExpression",id=getunique(1), rest ="operator=""CAST_OPERATOR""");
	  xmicode=[open_e;code1;open_t;close_t;close_e]
	else
	  printf("wrong datatype for convert\n");pause ;
	end
      elseif t(1)=="double" then
	//-- function double
	in1=t(2)(1)
	// r="("+t(1)+") ("+print_expr(in1)+")"; //r="("+r+")" 
	code1=print_expr(in1,etype="expression");
	[open1,close1]=codemodel_Expression(etype=etype,type="ParenthesisExpression",id=getunique(1));
	xmicode=[open1;"  "+code1;close1];
      elseif or(t(1)==["^",".^"]) then
	//-- pow operator 
	in1=t(2)(1);in2=t(2)(2);
	// r="pow("+print_expr(in1)+","+print_expr(in2)+")";
	operator=sprintf("%s_OPERATOR","POWER");
	in1=t(2)(1);in2=t(2)(2);
	// r=print_expr(in1)+t(1)+print_expr(in2); r="("+r+")" 
	code1=print_expr(in1,etype="leftArgument");
	code2=print_expr(in2,etype="rightArgument");
	[open2,close2]=codemodel_BinaryExpression(etype="expression",operator=operator,id=getunique(1));
	[open1,close1]=codemodel_Expression(etype=etype,type="ParenthesisExpression",id=getunique(1));
	xmicode=[open1;"  "+open2;"    "+code1;"    "+code2;"  "+close2;close1];
      elseif t(1)=="call" then
	// list("call,list(fname,outs,ins))
	// generate a call to fname (void) f (....) 
	// assuming that arguments are passed by ref
	fname=t(2)(1);outs=t(2)(2);ins=t(2)(3);
	s_outs=m2s(zeros(0,1));
	for e=outs ; 
	  if type(e,'short')=="bvar" then
	    if size(e,'*') == 1 &&  ~is_function_argument(e.get_varname[]) then 
	      // need to take the address 
	      [opene,closee]=codemodel_UnaryExpression(etype="arguments",operator="REF_OPERATOR",id=getunique(1));
	      [openv,closev]=codemodel_VariableExpression(etype="argument",variable=e.get_varname[]);
	      open = [opene;"  "+openv];
	      close=["  "+closev;closee];
	    else
	      [open,close]=codemodel_VariableExpression(etype="arguments",variable=e.get_varname[]);
	    end
	    s_outs.concatd[[open;close]];
	  elseif type(e,'short') == 's' then 
	    // s_outs.concatr[e];
	    pause tobedone 
	  else
	    // s_outs.concatr[string(e)];
	    pause tobedone 
	  end
	end
	s_ins=m2s(ones(0,1));
	for e=ins ; 
	  if type(e,'short')=="bvar" then
	    // si taille == 1 il faut generer un pointeur sauf si c'est
	    // un fonction argument 
	    if size(e,'*') == 1 &&  ~is_function_argument(e.get_varname[]) then 
	      // need to take the address 
	      [opene,closee]=codemodel_UnaryExpression(etype="arguments",operator="REF_OPERATOR",id=getunique(1));
	      [openv,closev]=codemodel_VariableExpression(etype="argument",variable=e.get_varname[]);
	      open = [opene;"  "+openv];
	      close=["  "+closev;closee];
	    else
	      [open,close]=codemodel_VariableExpression(etype="arguments",variable=e.get_varname[]);
	    end
	    s_ins.concatd[[open;close]];
	  elseif type(e,'short') == 's' then 
	    // s_ins.concatr[e];
	    pause tobedone 
	  else
	    // s_ins.concatr[string(e)];
	    pause tobedone
	  end
	end; 
	arguments=m2s(ones(0,1));
	if ~isempty(s_outs) then arguments.concatd["    "+s_outs];end
	if ~isempty(s_ins) then arguments.concatd["    "+s_ins];end
	[open_s,close_s]=codemodel_statements(type="ExpressionStatement", id=getunique(1),etype="statements");
	rest = sprintf('name=""%s"" standardFunction=""NONE""',fname);
	[open_e,close_e]=codemodel_Expression(etype="expression",type="CallExpression",id=getunique(1),rest=rest)
	// inserer les expressions de type argument 
	xmicode=[open_s;"  "+open_e;arguments;"  "+close_e;close_s];
	// insert call in _lib_defs
	global(_lib_defs=hash(10));
	_lib_defs(fname)=%t;
	
      elseif t(1)=="value" then
	// ? 
        val=t(2);
        xmicode=print_expr(val(1),etype=etype);
      else
	printf('unknown operator '+t(1)+'\n'),pause
      end
    else
      printf('unknown '+opt),pause
    end
  endfunction
  
  function [txt]=pcode_extract2(var,i,delims=["(",")"],tag="tag")
  // extraction var(i) 
  // we have to take care that i can be an expression 
  // and in that case we must build new expressions i.e i-1 !
    if is_num(i) then
      idx=string(valueof(i)-1)
      [open1,close1]=codemodel_VariableExpression(etype=tag,variable=var.get_varname[],index=idx);
      txt = [open1;close1];
    else 
      // here it's more complex we must build a -1 expression 
      newi= expression("-",list(i,1i),"overflow");
      idx=print_expr(newi,etype="indexExpressions");
      [open1,close1]=codemodel_VariableExpression(etype=tag,variable=var.get_varname[]);
      txt = [open1;"  "+idx;close1];
    end
  endfunction
  
  function [txt,id]=pcode_extract3(var,i,j,delims=["(",")"],tag="tag")
  // extraction var(i,j);
  // take care that i and j can be an expressions 
  // and in that case we must build new expressions i.e i-1 !
    sz=size(var); // size works for bvar 
    if is_num(i) & is_num(j) then
      i=valueof(i);j=valueof(j);
      idx=string((j-1)*sz(1)+i-1)
      [open1,close1]=codemodel_VariableExpression(etype=tag,variable=var.get_varname[],index=idx);
      txt = [open1;close1];
    elseif ~is_num(i) & is_num(j) then
      ioff=(valueof(j)-1)*sz(1);
      exp1 = expression("+",list(i,ioff),"overflow");
      idx=print_expr(exp1,etype="indexExpressions");
      [open1,close1]=codemodel_VariableExpression(etype=tag,variable=var.get_varname[]);
      txt = [open1;"  "+idx;close1];
    elseif is_num(i) & ~is_num(j) then
      v1 = valueof(i)-1;
      exp2 = expression("-",list(j,1i),"overflow");
      exp3 = expression("*",list(exp2,sz(1)),"overflow");
      exp4 = expression("+",list(exp3,v1),"overflow");
      idx=print_expr(exp4,etype="indexExpressions");
      [open1,close1]=codemodel_VariableExpression(etype=tag,variable=var.get_varname[]);
      txt = [open1;"  "+idx;close1];
    else
      // idx=sprintf("%s-1+%s*(%s-1)",print_expr(i),string(sz(1)),print_expr(j));
      exp1 = expression("-",list(i,1i),"overflow");
      exp2 = expression("-",list(j,1i),"overflow");
      exp3 = expression("*",list(exp2,sz(1)),"overflow");
      exp4 = expression("+",list(exp3,exp1),"overflow");
      idx=print_expr(exp4,etype="indexExpressions");
      [open1,close1]=codemodel_VariableExpression(etype=tag,variable=var.get_varname[]);
      txt = [open1;"  "+idx;close1];
    end
  endfunction
  
  function y=ending()
    y=";"
  endfunction

  function funlist=fun_sat_defs( )
    
    function ty=nextCtypeof(dt)
      X=["_int8",  "_uint8",  "_int16", "_uint16", "_int32", "_uint32"]
      Y=["int16_t","uint16_t","int32_t","uint32_t","int64_t","uint64_t"]
      k=find(dt==X);
      if isempty(k) then pause,error('Unsupported data type: '+dt), else ty=Y(k);end
    endfunction
    
    funlist=struct();
    types=["_int8","_uint8","_int16","_uint16","_int32","_uint32"
	   "INT8_MAX", "UINT8_MAX","INT16_MAX","UINT16_MAX", "INT32_MAX","UINT32_MAX"
	   "INT8_MIN", "0",  "INT16_MIN","0", "INT32_MIN","0"]
    ops2=["add","sub","mult"
	  "+", "-",  "*"  ];
    for ty=types
      for op=ops2
	txt=[Ctypeof(ty(1))+" "+op(1)+ty(1)+"_satur("+Ctypeof(ty(1))+" x,"+Ctypeof(ty(1))+" y) {"
	     nextCtypeof(ty(1))+" z;"
	     "z=("+nextCtypeof(ty(1))+") x"+op(2)+"y;"
	     "if (z>("+nextCtypeof(ty(1))+")"+ty(2)+") return "+ty(2)+";"
	     "if (z<("+nextCtypeof(ty(1))+")"+ty(3)+") return "+ty(3)+";"
	     "return ("+Ctypeof(ty(1))+")z;"
	     "}"
	     ""]
	funlist(op(1)+ty(1)+"_satur")=txt
      end
    end

    stypes=["_int8","_int16","_int32";  
	    "INT8_MAX","INT16_MAX", "INT32_MAX";
	    "INT8_MIN", "INT16_MIN", "INT32_MIN"]
    utypes=["_uint8","_uint16", "_uint32";
	    "UINT8_MAX","UINT16_MAX", "UINT32_MAX"]
    for ty=stypes  
      txt="#define neg"+ty(1)+"_satur(ZZ)  (ZZ=="+ty(3)+") ? "+ty(2)+" : -ZZ "
      funlist("neg"+ty(1)+"_satur")=txt  
    end
    for ty=utypes  
      txt="#define neg"+ty(1)+"_satur(ZZ) 0;"
      funlist("neg"+ty(1)+"_satur")=txt  
    end
  endfunction
  
  function [txt,extra_code]=print_var(vartype,var,referencedBy="")
  // declaration of a constant or persistent variable 
    extra_code=m2s(zeros(0,1));
    global(Refs=hash(10)); 
    txt=m2s([]);
    typ=datatype(var);sz=size(valueof(var));val=valueof(var);val=val(:);name=var.get_varname[];
    typ=Ctypeof(typ);
    static=vartype=='persistent'
    if prod(sz)==1 then
      msg = sprintf("declaration %s %s=%s;",typ,name,string(val));
      txt.concatd[print_var_constant(var,referencedBy=Refs.find[name,def=''],msg=msg,static=static)];
    else
      [save_field,save_prec,save_eflag]=format("get"); 
      format("long");
      if type(val,'short')== 'i' then val = i2m(val);end
      values = strsubst(strsubst(sprint(val',as_read=%t),"[",""),"]","");
      format(save_field,save_prec,save_eflag);
      values = strsubst(values,"...","\n");
      values = catenate(values,sep="");
      msg =sprintf("%s %s[]={%s};",typ,name,values); 
      txt.concatd[print_var_constant(var,referencedBy=Refs.find[name,def=''],msg=msg,static=static)];
    end
  endfunction
  
  function txt=print_var_constant(var,referencedBy="",msg="",static=%f)
  // une variable de type array qui a une valeur d'initialization 
    txt=sprintf( "<!-- declaration for variable %s -->",name=var.get_varname[]);
    if ~msg.equal[""] then 
      [open_a,close_a]=codemodel_annotations(msg=msg,id=getunique(1));
      txt.concatd[[open_a;close_a]];
    end
    if static then static_s="true";else  static_s="false";end;
    [open1,close1]=codemodel_Variable(id=var.get_varname[],name=var.get_varname[],referencedBy=referencedBy,isStatic=static_s)
    typ=datatype(var);val=var.get_value[];sz=size(val); 
    // XXXXX get proper types 
    if prod(sz)<> 1 then 
      [open2,close2]=codemodel_dataType(etype="dataType",type="TArray",id=getunique(1));
      txt_d=codemodel_type(var.get_value[],etype="baseType")
      [open4,close4]=codemodel_IntegerExpression(prod(sz),etype="dimensions",id=getunique(1));
      txt_val=codemodel_initialValue(val);
      txt.concatd[[open1;"  "+open2;"    "+txt_d;"    "+open4;"    "+close4;"  "+close2;"  "+txt_val;close1]];
    else
      // XXX: should we declare here a 1x1 array for consistency or just
      // a scalar ? 
      if %t then 
	txt_d=codemodel_type(var.get_value[],etype="dataType")
	txt_val=codemodel_initialValue_scalar(val);
	txt.concatd[[open1;"  "+txt_d;"  "+txt_val;close1]];
      end
      if %f then 
	// as a 1x1 array 
	[open2,close2]=codemodel_dataType(etype="dataType",type="TArray",id=getunique(1));
	txt_d=codemodel_type(var.get_value[],etype="baseType")
	[open4,close4]=codemodel_IntegerExpression(prod(sz),etype="dimensions",id=getunique(1));
	txt_val=codemodel_initialValue(val);
	txt.concatd[[open1;"  "+open2;"    "+txt_d;"    "+open4;"    "+close4;"  "+close2;"  "+txt_val;close1]];
      end
    end
  endfunction
  
  function txt=print_var_symbolic(var,referencedBy="")
    open_c=sprintf( "<!-- declaration for variable %s -->",name=var.get_varname[]);
    [open1,close1]=codemodel_Variable(id=var.get_varname[],name=var.get_varname[],referencedBy=referencedBy)
    typ=datatype(var);val=var.get_value[];sz=size(val); 
    if prod(sz)<> 1 then 
      [open2,close2]=codemodel_dataType(etype="dataType",type="TArray",id=getunique(1));
      txt_d=codemodel_type(var.get_value[],etype="baseType")
      [open4,close4]=codemodel_IntegerExpression(prod(sz),etype="dimensions",id=getunique(1));
      txt = [open_c;open1;"  "+open2;"    "+txt_d;"    "+open4;"    "+close4;"  "+close2;close1];
    else
      txt_d=codemodel_type(var.get_value[],etype="dataType")
      txt = [open_c;open1;"  "+txt_d;close1];
    end
  endfunction

  function txt=print_set_statement(var,rhs)
  // a set
    if rhs.exp(1)=="value" then 
      if prod(size(valueof(var)))>1 then 
	// XXXX: here it could be good to have a declaration to initialize
	// cons as a static variable and then a loop for copying.
	for kj=1:prod(size(valueof(var)))
	  cons=valueof(rhs.exp(2)(1));
	  [open_a,close_a]=codemodel_statements(type="AssignStatement", id=getunique(1), rest="operator=""SIMPLE_ASSIGN""");
	  [open_l,close_l]=codemodel_VariableExpression(etype="leftExpression",variable=var.get_varname[],index=kj-1);
	  rhsstr=print_expr(cons(kj),etype="rightExpression");
	  txt=[open_a;"  "+open_l;"  "+close_l;"  "+rhsstr;close_a];
	end
      else
	// an assign again var = rhs.exp(2)(1)
	index=[];if  is_function_argument(var.get_varname[]) then index = 0;end
	[open_a,close_a]=codemodel_statements(type="AssignStatement", id=getunique(1), rest="operator=""SIMPLE_ASSIGN""");
	[open_l,close_l]=codemodel_VariableExpression(etype="leftExpression",variable=var.get_varname[],index=index);
	rhsstr=print_expr(rhs.exp(2)(1),etype="rightExpression");
	txt=[open_a;"  "+open_l;"  "+close_l;"  "+rhsstr;close_a];
      end
    elseif rhs.exp(1)=="If_exp"
      //-- an if expression if condi then var=in1 else var = in2 end
      condi=rhs.exp(2)(1);in1=rhs.exp(2)(2);in2=rhs.exp(2)(3);
      if prod(size(valueof(in1)))==1 then
	[open_if,close_if]=codemodel_statements(etype="statements",type="IfStatement", id=getunique(1));
	xmi_cond= print_expr(condi,etype="conditionExpression");
	xmi_then=codemodel_AssignStatement_scalar(var,in1,etype="thenStatement");
	xmi_else=codemodel_AssignStatement_scalar(var,in2,etype="elseStatement");
	txt = [open_if;"  "+xmi_cond;"  "+xmi_else;"  "+xmi_then;close_if];
      else
	[open_if,close_if]=codemodel_statements(etype="statements",type="IfStatement", id=getunique(1));
	xmi_cond= print_expr(condi,etype="conditionExpression");
	// <thenStatement xmi:type="geneauto.emf.models.gacodemodel.statement:CompoundStatement"
	[open_then,close_then]=codemodel_statements(etype="thenStatement",type="CompoundStatement", id=getunique(1));
	xmi_then_st=codemodel_AssignStatement(var,in1,etype="statements");
	xmi_then =[open_then;"  "+xmi_then_st;close_then];
	[open_else,close_else]=codemodel_statements(etype="elseStatement",type="CompoundStatement", id=getunique(1));
	xmi_else_st=codemodel_AssignStatement(var,in2,etype="statements");
	xmi_else =[open_else;"  "+xmi_else_st;close_else];
	txt= [open_if;"  "+xmi_cond;"  "+xmi_else;"  "+xmi_then;close_if];
      end
    elseif rhs.exp(1)=="Select_exp"
      // a CaseStatement in xmi 
      iter_var=symbolics(1i);
      indi=symbolics(1i);
      code_declaration_insert('ephemere',indi);
      ncases=length(rhs.exp(2))-1
      condi=rhs.exp(2)(1);
      nz=size(var,'*');
      // icase = condi[indi]; 
      icase=symbolics(1i);
      code_declaration_insert('ephemere',icase);
      allwhencases = m2s(zeros(0,1));
      for casei=1:ncases
	// case condition 
	[open_when,close_when]=codemodel_IntegerExpression(m2i(casei),etype="when",id=getunique(1));
	ini=rhs.exp(2)(casei+1);
	if type(ini,'short')<>'bvar' then 
	  ini = numerics(ini);
	  code_declaration_insert('constant',ini);
	end
	if %f && size(ini,'*')==1 then
	  xmi_sts=codemodel_AssignStatement_scalar(var,ini,etype="statements");
	else
	  // var[icase]=ini[icase];
	  [open_a,close_a]=codemodel_statements(type="AssignStatement", id=getunique(1), rest="operator=""SIMPLE_ASSIGN""");
	  [open_l,close_l]=codemodel_VariableExpression(etype="leftExpression",variable=var.get_varname[],index=iter_var);
	  [open_r,close_r]=codemodel_VariableExpression(etype="rightExpression",variable=ini.get_varname[],index=iter_var);
	  xmi_sts=[open_a;"  "+open_l;"  "+close_l;"  "+open_r;"  "+close_r;close_a];
	end
	[open_whencases,close_whencases]=codemodel_statements(etype="whenCases",type="WhenCase", id=getunique(1));
	whencases = [open_whencases;"  "+xmi_sts; "  "+open_when;"  "+close_when;close_whencases]
	allwhencases.concatd[whencases]
      end
      [open_default,close_default]=codemodel_statements(etype="defaultCase",type="DefaultCase", id=getunique(1));
      [open_blank,close_blank]=codemodel_statements(etype="statements",type="BlankStatement", id=getunique(1));
      allwhencases.concatd[[open_default;"  "+open_blank;"  "+close_blank;close_default]];
      xmi_cond= print_expr(icase,etype="condition");
      [open_case,close_case]=codemodel_statements(etype="statements",type="CaseStatement", id=getunique(1));
      cases_body=[open_case;"  "+xmi_cond;"  "+allwhencases;close_case];
      // variable assignment before the switch 
      [open_s,close_s]=codemodel_statements(type="AssignStatement", id=getunique(1), rest="operator=""SIMPLE_ASSIGN""");
      [xmileft]=print_expr(icase,etype="leftExpression");
      // when condi is a scalar then nz-1 should be 0 
      // we need here a min max 
      
      if %t then 
	// we build a max(1,min(sz,.)) 
	fun="max";rest = sprintf('name=""%s"" standardFunction=""%s_FUN""',fun, toupper(fun));
	[open1,close1]=codemodel_Expression(etype="rightExpression",type="CallExpression",id=getunique(1),rest=rest)
	xmiarg1=print_expr(numerics(1i),etype="arguments");
	fun="min";rest = sprintf('name=""%s"" standardFunction=""%s_FUN""',fun, toupper(fun));
	[open2,close2]=codemodel_Expression(etype="arguments",type="CallExpression",id=getunique(1),rest=rest)
	xmiarg2_1=print_expr(numerics(m2i(ncases)),etype="arguments");
	if size(condi,'*') == 1 &&  ~is_function_argument(condi.get_varname[]) then 
	  // we could get rid of the loop 
	  [open_r,close_r]=codemodel_VariableExpression(etype="arguments",variable=condi.get_varname[],index=[]);
	else
	  [open_r,close_r]=codemodel_VariableExpression(etype="arguments",variable=condi.get_varname[],index=iter_var);
	end
	xmiarg2=[open2;"  "+xmiarg2_1;"  "+open_r;"  "+close_r;close2];
	open_r =[open1];
	close_r=["  "+xmiarg1;"  "+xmiarg2;close1];
      else
	if size(condi,'*') == 1 &&  ~is_function_argument(condi.get_varname[]) then 
	  // we could get rid of the loop 
	  [open_r,close_r]=codemodel_VariableExpression(etype="rightExpression",variable=condi.get_varname[],index=[]);
	else
	  [open_r,close_r]=codemodel_VariableExpression(etype="rightExpression",variable=condi.get_varname[],index=iter_var);
	end
      end
      txt_assign= [open_s;"  "+xmileft;"  "+open_r;"  "+close_r;close_s];
      txt=codemodel_RangeIterationStatement(iter_var,0,nz-1,[txt_assign;cases_body],etype="statements");
    else
      // ?
      if prod(size(valueof(var)))>1 then 
	pause to_be_done
	for kj=1:prod(size(valueof(var)))
	  cons=valueof(rhs);
	  txt =Ind+var.get_varname[]+"("+string(kj)+")="+string(cons(kj)) +ending();
	end
      else
	// we have to check if var is a function argument or not 
	// since we generate an assignement var = rhs or var[0]=rhs
	// depending on var status 
	index=[];
	if  is_function_argument(var.get_varname[]) then index = 0;end
	[open_a,close_a]=codemodel_statements(type="AssignStatement", id=getunique(1), rest="operator=""SIMPLE_ASSIGN""");
	[open_l,close_l]=codemodel_VariableExpression(etype="leftExpression",variable= var.get_varname[],index=index);
	rhsstr=print_expr(rhs,etype="rightExpression");
	txt = [open_a;"  "+open_l;"  "+close_l;"  "+rhsstr;close_a];
      end
    end
  endfunction
  
  // produce an xmi version of the code 
  delims=["(",")"];
  
  txtdec=m2s(zeros(0,1))
  txtcode=m2s(zeros(0,1))
  txtextra=m2s(zeros(0,1));

  // Hash table to keep track of places where variables were used 
  global(Refs=hash(10)); 
  if ~keeprefs then 
    Refs=hash(10); // reinitialize Refs 
  end
  
  // print the code 
  Ind="   ";
  for m=1:length(code)
    elt=code(m);
    select elt(1) 
     case "nop" then
      // nothing to do 
     case "annotation" then
      // annotation 
      txtcode($+1,1)=Ind+"// "+elt(2)
     case "set" then
      var=elt(2);rhs=elt(3);
      txt=print_set_statement(var,rhs);
      txtcode.concatd[txt];
     case "bi_insert" then
      // var(i,j)=value
      var=elt(2);i=elt(3);j=elt(4);rhs=elt(5); 
      [open_s,close_s]=codemodel_statements(type="AssignStatement", id=getunique(1), rest="operator=""SIMPLE_ASSIGN""");
      [xmileft]=pcode_extract3(var,i,j,delims=delims,tag="leftExpression");
      [xmiright]=print_expr(rhs,etype="rightExpression");
      txtcode.concatd[[open_s;"  "+xmileft;"  "+xmiright;close_s]];
     case "uni_insert" then
      // var(i)=value
      var=elt(2);i=elt(3);rhs=elt(4);
      [open_s,close_s]=codemodel_statements(type="AssignStatement", id=getunique(1), rest="operator=""SIMPLE_ASSIGN""");
      [xmileft]=pcode_extract2(var,i,delims=delims,tag="leftExpression");
      [xmiright]=print_expr(rhs,etype="rightExpression");
      txtcode.concatd[[open_s;"  "+xmileft;"  "+xmiright;close_s]];
     case "assign" then
      // assign 
      var=elt(2);rhs=elt(3)
      if size(var,'*') > 1 then 
	// this can appear for first assignement of a non scalar variable
	// var[0]= rhs 
	[open_a,close_a]=codemodel_statements(type="AssignStatement", id=getunique(1), rest="operator=""SIMPLE_ASSIGN""");
	[open_l,close_l]=codemodel_VariableExpression(etype="leftExpression",variable=var.get_varname[],index=0);
	rhs_str=print_expr(rhs,etype="rightExpression");
	txtcode.concatd[[open_a;"  "+ open_l;"  "+close_l;"  "+rhs_str;close_a]];
      else 
	// here the rhs can be a value or an If_expr or a 
	if type(rhs,'short') <> 'h' then rhs=expression("value",list(rhs));end
	txt=print_set_statement(var,rhs);
	txtcode.concatd[txt];
      end
     case "mcopy" then
      // mcopy i.e make var = in 
      var=elt(2);in=elt(3);
      if type(in,'short')<>"bvar" then disp('wrong memcpy type'),pause, end
      in_name = in.get_varname[];
      if part(in_name,1)== '&' then 
	// here we must do var[0]=in
	in_name = part(in_name,2:length(in_name)); index=0;
      else
	index=[];
      end
      [open_a,close_a]=codemodel_statements(type="AssignStatement", id=getunique(1), rest="operator=""SIMPLE_ASSIGN""");
      [open_l,close_l]=codemodel_VariableExpression(etype="leftExpression",variable=var.get_varname[],index=index);
      [open_r,close_r]=codemodel_VariableExpression(etype="rightExpression",variable=in_name);
      txtcode.concatd[[open_a;"  "+ open_l;"  "+close_l;"  "+open_r;"  "+close_r;close_a]];
     case "if_expr" then
      // if elt(2) then elt(3) else elt(4)
      // elt(2) is an expression 
      // elt(3) and elt(4) are statements 
      [open_if,close_if]=codemodel_statements(etype="statements",type="IfStatement", id=getunique(1));
      xmi_cond= print_expr(elt(2),etype="conditionExpression");
      // <thenStatement xmi:type="geneauto.emf.models.gacodemodel.statement:CompoundStatement"
      [open_then,close_then]=codemodel_statements(etype="thenStatement",type="CompoundStatement", id=getunique(1));
      xmi_then_st= print_expr(elt(3),etype="statements");
      xmi_then =[open_then;"  "+xmi_then_st;close_then];
      [open_else,close_else]=codemodel_statements(etype="elseStatement",type="CompoundStatement", id=getunique(1));
      xmi_else_st= print_expr(elt(4),etype="statements");
      xmi_else =[open_else;"  "+xmi_else_st;close_else];
      txtcode.concatd[[open_if;"  "+xmi_cond;"  "+xmi_else;"  "+xmi_then;close_if]]
      // 
     case "ident" then
      // generate code for elt(2) 
      txtcode.concatd[print_expr(elt(2),etype="statements")];
      
     case "endfunction" then
      // we postpone later the function code generation 
      // but here we generate extra AssignStatement to match 
      // function arguments and code variables 
      // this code must be before declaration generation 
      // to have correct refs 
      fname=elt(2);io=elt(3);
      args= io.values;//io.__keys;
      // take care that names may have changed 
      // due to code evaluation 
      for i=1:size(args,'*')
	newname = zins(i).get_varname[];
	if ~newname.equal[args(i)] then 
	  // insert args(i) = newname(i) 
	  [open_a,close_a]=codemodel_statements(type="AssignStatement", id=getunique(1), rest="operator=""SIMPLE_ASSIGN""");
	  [open_l,close_l]=codemodel_VariableExpression(etype="leftExpression",variable=args(i));
	  [open_r,close_r]=codemodel_VariableExpression(etype="rightExpression",variable=zins(i).get_varname[]);
	  txtcode.concatd[[open_a;"  "+ open_l;"  "+close_l;"  "+open_r;"  "+close_r;close_a]];
	end
      end
     case "callf" then 
      // argument is an expression
      txtcode.concatd[print_expr(elt(2))];
    else
      printf("unknown operation '+elt(1));
      pause 
    end
  end
  
  for t=declarations
    select t(1) 
     case "nop" then 
     case "annotation" then 
      [open,close]=codemodel_annotations(msg=t(2),id=getunique(1))
      txtdec.concatd[[open;close]];
     case {'persistent','constant'} then
      if type(t(2),'short') == 'bvar' then 
	[txt,extra]=print_var(t(1),t(2));
	txtdec.concatd[txt];
	txtextra.concatd[extra];
      end
     case 'ephemere' then
      // declaration of an ephemeral variable 
      var=t(2);
      typ=datatype(var);val=var.get_value[];sz=size(val);
      if ~var.is_symbolic[] then 
	[txt,extra]=print_var('constant',t(2),referencedBy=Refs.find[var.get_varname[],def='']);
	txtdec.concatd[txt]; 
	txtextra.concatd[extra];
      else
	txtdec.concatd[print_var_symbolic(var,referencedBy=Refs.find[var.get_varname[],def=''])];
      end
     case 'function' then
      // unused the tag endfunction do the job
      pause function-part
      if %f then 
	fname=t(2);io=t(3);
	args= io.values; //io.__keys;
	sargs="";
	for i=1:size(args,'*')-1
	  elt = io(args(i));
	  if prod(size(valueof(elt))) > 1 then
	    nm="*"+args(i);
	  else
	    nm=args(i);
	  end
	  sargs=sargs+" "+Ctypeof(datatype(elt))+" "+nm+","
	end
	elt = io(args($));
	if prod(size(valueof(elt))) >1 then
	  nm="*"+args($);
	else
	  nm=args($);
	end
	sargs=sargs+" "+Ctypeof(datatype(elt))+" "+nm
	txtdec($+1,1)=""
	txtdec($+1,1)="void "+fname+"("+sargs+"){"
      end
    else
      error('sdfddsf')
    end
  end
  
  txtcode=[txtextra;txtcode];
  
  // last pass to encapsulate the code in a function declaration if requested 
  Ind="   ";
  for m=1:length(code)
    elt=code(m);
    select elt(1) 
     case "endfunction" then
      // close code for a function we are now able to generate the
      // function tag
      fname=elt(2);io=elt(3);
      txtcode= codemodel_Function(fname,txtcode,txtdec,io,zins,Lres2,is_in)
      txtdec =m2s(zeros(0,1));
    end
  end
endfunction


function funlist=code_printer_xmi_sat_defs( )

  function txt=codemodel_Function_dec(fname,return_type,arg_types)
  // full xmi code for a function declaration 

    function [txt]=codemodel_type_from_str(str,etype="dataType") 
    // generate a <etype xmi:type="geneauto.emf.models.gadatatypes:xxx" ...> 
    // ["int8","uint8","int16","uint16","int32","uint32"]
      vtype="TRealInteger";
      itype = str 
      itypes=["int8","uint8","int16","uint16","int32","uint32","int64","uint64"];
      signed= part(itype,0)<>"u"; 
      if signed then s_signed="true" else s_signed="false";end 
      nbits = strsubst(str,["int","u"],["",""])
      I=find(str == itypes);
      rest=sprintf("nBits=""%s"" signed=""%s"" ",nbits,s_signed);
      [open_d,close_d]=codemodel_dataType(etype=etype,type=vtype,id=getunique(1), rest =rest);
      txt = [open_d;close_d];
    endfunction

    function [txt,id]=codemodel_function_dec_argument(typ,sz,i,ref="")
      global(Refs=hash(10)); 
      id=getunique(1);
      name = sprintf("arg%d",i);
      open_c=sprintf( "<!-- declaration of function argument %s -->",name);
      [open1,close1]=codemodel_FunctionArgument(id=id,name=name,referencedBy=ref,direction="IN")
      if prod(sz)<> 1 then 
	// array 
	[open2,close2]=codemodel_dataType(etype="dataType",type="TArray",id=getunique(1));
	[basetype]=codemodel_type_from_str(typ,etype="baseType") 
	[open4,close4]=codemodel_IntegerExpression(prod(sz),etype="dimensions",id=getunique(1));
	txt = [open_c;open1;"  "+open2;"    "+basetype;"    "+open4;"    "+close4;"  "+close2;close1];
      else
	// 1x1 array for function argument of size 1
	[basetype]=codemodel_type_from_str(typ,etype="dataType") 
	txt = [open_c;open1;"    "+basetype;close1];
      end
    endfunction
    
    global(Refs=hash(10)); 
    // add arguments now that referencedBy are ok in Refs
    txt_vars=m2s(zeros(0,1));
    id_vars =m2s(zeros(0,1));
    for i=1:size(arg_types,'*') 
      [txt,id]=codemodel_function_dec_argument(arg_types(i),1,i);
      txt_vars.concatd[txt];
      id_vars.concatd[id];
    end
    // type for returned value 
    [txt_ret]=codemodel_type_from_str(return_type,etype="dataType");
    // function code with proper arguments 
    rest=sprintf("calledBy=""%s""",Refs.find[fname,def=' ']);	  
    [openf,closef]=codemodel_functions(name=fname,id=getunique(1), arguments=catenate(id_vars,sep=" "),rest=rest);
    [openb,closeb]=codemodel_body(id=getunique(1));
    txt=[openf;
	 "  "+openb;
	 "  "+closeb;
	 "  "+txt_ret;
	 "  "+txt_vars;
	 closef];
  endfunction
    
  funlist=hash(10);
  types=["int8","uint8","int16","uint16","int32","uint32"]
  ops2=["add","sub","mult"]
  for ty=types
    for op=ops2
      fname= op(1)+"_"+ty(1)+"_satur";
      arg_type = ty(1);
      txt=codemodel_Function_dec(fname,arg_type,[arg_type, arg_type]);
      funlist(fname)=txt
    end
  end
      
  for ty=types  
    fname="neg"+"_"+ty(1)+"_satur";
    arg_type = ty(1);
    txt=codemodel_Function_dec(fname,arg_type,[arg_type]);
    funlist(fname)=txt;

    fname=ty(1)+"_satur";
    arg_type = ty(1);
    txt=codemodel_Function_dec(fname,arg_type,[arg_type]);
    funlist(fname)=txt;
  end
  
endfunction

