// Copyright 2013-2015 Enpc, Altair Engineering Inc
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
// Authors: J.-Ph. Chancelier (Cermics/ENPC), R. Nikoukhah (Altair Engineering)


function txt=code_printer_c(code,declarations)
// translate pseudo code into C code in txt
// 

  function r=print_string(v)
    
    function s=supress_trailing_zeros (s)
      n=length(s);
      while part(s,n)=="0" then
	n = n-1;
	s= part(s,[1:n]);
      end
    endfunction
    
    [a,b,c]=format("get");
    format("long");
    r=string(v);       
    indr=strindex(r,".")
    if ~isempty(indr) then
      r=supress_trailing_zeros (r)
    end
    format(a,b,c);
  endfunction
  
  function ty=Ctypeof(dt)
    ee=["int8","uint8","int16","uint16","int32","uint32","double","complex","boolean","b"]
    // ef=["int8_t","uint8_t","int16_t","uint16_t","int32_t","uint32_t","double","double","gboolean","gboolean"]
    // booleans are int (should be improved with typedef).
    ef=["int8_t","uint8_t","int16_t","uint16_t","int32_t","uint32_t","double","double","int","int"]
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
      
  function r = print_expr(rhs)
  // print an expression 
  // _defs is used to collect a set of function which are to be added 
  // in generated code 
    global(_defs=m2s([]));
    global(_lib_defs=hash(10));
    //if type(_defs,'short') <> 'h' then _defs=hash(10);end 
    tps=["double","complex","boolean","b", "int8","uint8","int16",...
	 "uint16","int32","uint32"];
    opt = type(rhs,'short');
    if opt == 'h' then opt = rhs.type; end
    if or(opt==tps) then
      // ? 
      r=print_string(valueof(rhs));
      if opt == "b" then r =  strsubst(r,["T","F"],["TRUE","FALSE"]);end;
    elseif or(opt == ["m","i","b"]) then
      r=print_string(rhs);
      if opt == "b" then r =  strsubst(r,["T","F"],["TRUE","FALSE"]);end;
    elseif opt=="bvar" then
      // This should be the only case.
      if rhs.is_symbolic[] then 
	// if rhs is a function argument we should use * 
	r = rhs.get_varname[];
 	if size(rhs.get_value[],'*')== 1 &&  is_function_argument(r) then 
	  r='*'+ r;
	end
      else
	// sans doute à revoir XXX 
	r=print_string(rhs.get_value[]);
      end
    elseif opt=="op" then
      t=rhs.exp
      if t(1)=="b_extract" then
	u=t(2)(1);i=t(2)(2);j=t(2)(3)
	r=sprintf("(%s)",pcode_extract3(u,i,j,delims=delims));
      elseif t(1)=="u_extract" then
	u=t(2)(1);i=t(2)(2);
	r=sprintf("(%s)",pcode_extract2(u,i));
      elseif or(t(1)==["==","<=",">=","<",">"]) then
	in1=t(2)(1);in2=t(2)(2);
	r=print_expr(in1)+t(1)+print_expr(in2); r="("+r+")"
      elseif or(t(1)==["<>"]) then
         in1=t(2)(1);in2=t(2)(2);
         r=print_expr(in1)+"!="+print_expr(in2); r="("+r+")"
      elseif or(t(1)==["&"]) then
         in1=t(2)(1);in2=t(2)(2);
         r=print_expr(in1)+"&&"+print_expr(in2); r="("+r+")"
      elseif or(t(1)==["|"]) then
         in1=t(2)(1);in2=t(2)(2);
         r=print_expr(in1)+"||"+print_expr(in2); r="("+r+")"
      elseif or(t(1)==["/","./"]) then
	// take care that /* is not correct 
	in1=t(2)(1);in2=t(2)(2);
	r=print_expr(in1)+"/ "+print_expr(in2); r="("+r+")"
      elseif or(t(1)==["\"]) then
	// TBD ".\" 
	in1=t(2)(1);in2=t(2)(2);
	r=print_expr(in2)+"/ "+print_expr(in1); r="("+r+")"
      elseif or(t(1)==["+"]) then
	in1=t(2)(1);in2=t(2)(2);
	if datatype(in1)=="double" | (isempty(t(3)) || t(3)=="overflow") then
	  r=print_expr(in1)+t(1)+print_expr(in2); r="("+r+")"
	else
	  r="add_"+datatype(in1)+"_"+t(3)+"("+print_expr(in1)+","+print_expr(in2)+")";
	  _defs=unique([_defs;"add_"+datatype(in1)+"_"+t(3)]);
	end
      elseif or(t(1)==["*",".*"]) then
	in1=t(2)(1);in2=t(2)(2);
	if datatype(in1)=="double" | ~t(3).equal["satur"] then
	  r=print_expr(in1)+"*"+print_expr(in2); r="("+r+")"
	else
	  r="mult_"+datatype(in1)+"_"+t(3)+"("+print_expr(in1)+","+print_expr(in2)+")";
	  _defs=unique([_defs;"mult_"+datatype(in1)+"_"+t(3)]);
	end
      elseif t(1)=="-" then
	ins=t(2);
	if length(ins)==1 then
	  in1=ins(1)
	  if datatype(in1)=="double" | ~t(3).equal["satur"] then
	    r="-"+print_expr(in1); r="("+r+")"
	  else
	    r="neg_"+datatype(in1)+"_"+t(3)+"("+print_expr(in1)+","+print_expr(in2)+")";r="("+r+")"
	    _defs=unique([_defs;"neg_"+datatype(in1)+"_"+t(3)]);
	  end
	else
	  in1=ins(1);
	  in2=ins(2);     
	  r=print_expr(in1)+"-"+print_expr(in2); r="("+r+")" 
	end
      elseif or(t(1)==["sin","cos","tan","asin","acos","atan","sinh","cosh","tanh","sqrt","log","exp","floor","ceil"]) then
	in1=t(2)(1)
	r=t(1)+"("+print_expr(in1)+")"; //r="("+r+")" 
	if t(1)=="atan" && length(t(2))==2 then 
	  // atan can be of arity 2 and is called atan2 in C
	  r="atan2("+print_expr(in1)+","+print_expr(t(2)(2))+")";
	end
      elseif or(t(1)==["int8","uint8","int16","uint16","int32","uint32"]) then
	// convert to int 
	in1=t(2)(1)
        if isempty(t(3)) || t(3)=="overflow" then
            r="("+Ctypeof(t(1))+") ("++print_expr(in1)+")";
        else
            r=t(1)+"_"+t(3)+"("+print_expr(in1)+")"; //r="("+r+")"
            _defs=unique([_defs;t(1)+"_"+t(3)])
        end  
      elseif or(t(1)==["boolean","b"]) then
	// convert to boolean 
	in1 = t(2)(1); typ = t(2)(2).get_value[]; r= print_expr(in1);
	select typ 
	 case "double" then r=sprintf("( %s != 0.0)",r);
	 case {"int8","uint8","int16","uint16","int32","uint32"} then 
	  r=sprintf("(%s != ((%s) 0));",r,typ);
	 case "b" then r=r;
	else
	  printf("wrong datatype for convert\n");pause ;
	end
      elseif t(1)=="double" then
	in1=t(2)(1)
	r="("+t(1)+") ("+print_expr(in1)+")"; //r="("+r+")" 
      elseif or(t(1)==["^",".^"]) then
	in1=t(2)(1);in2=t(2)(2);
	r="pow("+print_expr(in1)+","+print_expr(in2)+")";
      elseif t(1)=="call" then
	// list("call",list(fname,outs,ins))
	fname=t(2)(1);outs=t(2)(2);ins=t(2)(3);
	s_outs=m2s([]);
	for e=outs ; 
	  if type(e,'short')=="bvar" then
            nm = e.get_varname[];
	      s_outs.concatr[nm];
	  elseif type(e,'short') == 's' then 
	    s_outs.concatr[e];
	  else
	    s_outs.concatr[print_string(e)];
	  end
	end
	s_ins=m2s([]);
	for e=ins ; 
	  if type(e,'short')=="bvar" then
	    nm = e.get_varname[];
	      s_ins.concatr[nm];
	  elseif  type(e,'short') == 's' then 
	    s_ins.concatr[e];
	  else
	    s_ins.concatr[print_string(e)];
	  end
	end; 
	r = sprintf('%s(%s);',fname,catenate([s_outs,s_ins],sep=",")); 
	// insert call in _lib_defs 
	_lib_defs(fname)=%t;
      elseif t(1)=="value" then
        val=t(2);
        r=print_expr(val(1))
      else
	printf('unknown operator '+t(1)+'\n'),pause
      end
    else
      printf('unknown '+opt),pause
    end
  endfunction

  function txt=pcode_extract2(var,i,delims=["[","]"])
  // extraction var(i) i peut-etre une expression
    if is_num(i) then
      idx=print_string(valueof(i)-1)
    else
      idx=print_expr(i)+print_string(-1)
    end
    txt=var.get_varname[]+delims(1)+idx+delims(2);
  endfunction

  function txt=pcode_extract3(var,i,j,delims=["[","]"])
  // extraction var(i,j)
    sz=size(valueof(var));
    if is_num(i) & is_num(j) then
      i=valueof(i);j=valueof(j);
      idx=print_string((j-1)*sz(1)+i-1)
    elseif ~is_num(i) & is_num(j) then
      j=valueof(j)
      idx=print_expr(i)+"+"+print_string(-1+(j-1)*sz(1))
    elseif is_num(i) & ~is_num(j) then
      i=valueof(i)
      idx=print_string(i-1)+"+"+print_string(sz(1))+"*("+print_expr(j)+"-1)"
    else
      idx=sprintf("%s-1+%s*(%s-1)",print_expr(i),print_string(sz(1)),print_expr(j));
    end
    txt=sprintf("%s%s%s%s",var.get_varname[],delims(1),idx,delims(2));
  endfunction

  function y=ending()
    y=";"
  endfunction
  
  function str=sizeof(dt)
    str = sprintf("sizeof(%s)",Ctypeof(dt));
  endfunction

  function txt=print_var(vartype,var)
  // generates a declaration for a variable 
  // and initialization using the value of var 
  // if vartype is 'persistent' then the variable is declared static 
    txt=m2s(zeros(0,1));
    typ=datatype(var);sz=size(valueof(var));val=valueof(var);val=val(:);name=var.get_varname[];
    typ=Ctypeof(typ);
    if vartype=='persistent' then typ = "static "+typ+" ";end
    if prod(sz)==1 then
      // declaration inserted 
      str=strsubst(print_string(val),["T","F"],["TRUE","FALSE"]);
      txt($+1,1)=sprintf("%s %s=%s;",typ,name,str);
    else
      //save current format
      [save_field,save_prec,save_eflag]=format("get"); 
      format("long");
      if type(val,'short')== 'i' then val = i2m(val);end
      values = strsubst(sprint(val',as_read=%t),["[","]","%t","%f"],["","","TRUE","FALSE"]);
      format(save_field,save_prec,save_eflag);
      values = strsubst(values,"...","\n");
      values = catenate(values,sep="");
      txt($+1,1)=sprintf("%s %s[]={%s};",typ,name,values);
    end
  endfunction

  function txt=print_set_statement(var,rhs)
    txt=m2s(zeros(0,1));
    if rhs.exp(1)=="value" then 
      if prod(size(valueof(var))) > 1 then 
	// XXXX: here it could be good to have a declaration to initialize
	// cons as a static variable and then a loop for copying.
	for kj=1:prod(size(valueof(var)))
	  cons=valueof(rhs.exp(2)(1));
	  txt($+1,1)=Ind+var.get_varname[]+"["+print_string(kj-1)+"]="+print_string(cons(kj)) +ending()
	end
      else
	// 
	rhsstr=print_expr(rhs.exp(2)(1))
	r=var.get_varname[];if is_function_argument(r) then r = '*'+r;end;
	txt($+1,1)=Ind+r+"="+rhsstr +ending()
      end
    elseif rhs.exp(1)=="If_exp"
      condi=rhs.exp(2)(1);in1=rhs.exp(2)(2);in2=rhs.exp(2)(3);
      if prod(size(valueof(var))) == 1 then
	r=var.get_varname[];if is_function_argument(r) then r = '*'+r;end;
	txt($+1,1)=Ind+"if ("+print_expr(condi)+") {"
	txt($+1,1)=Ind+"  "+r+"="+print_expr(in1)+ending()
	txt($+1,1)=Ind+"} else {"
	txt($+1,1)=Ind+"  "+r+"="+print_expr(in2)+ending()
	txt($+1,1)=Ind+"}"
      else
	txt($+1,1)=Ind+"if ("+print_expr(condi)+") {"
	if type(in1,'short')<> 'bvar' then 
	  in1 = numerics(in1);
	  txt.concatd[Ind+Ind+print_var('constant',in1)];
	end
	txt($+1,1)=Ind+sprintf("  memcpy(%s,%s,%d*%s);",var.get_varname[],in1.get_varname[],...
			       prod(size(valueof(var))),sizeof(datatype(var)));
	txt($+1,1)=Ind+"} else {"
	if type(in2,'short')<> 'bvar' then 
	  in2 = numerics(in2);
	  txt.concatd[Ind+Ind+print_var('constant',in2)];
	end
	txt($+1,1)=Ind+sprintf("  memcpy(%s,%s,%d*%s);",var.get_varname[],in2.get_varname[],...
			       prod(size(valueof(var))),sizeof(datatype(var)));
	txt($+1,1)=Ind+"}"
      end
    elseif rhs.exp(1)=="Select_exp"
      indi=symbolics(1i);
      code_declaration_insert('ephemere',indi);
      ncases=length(rhs.exp(2))-1
      condi=rhs.exp(2)(1);
      nz=size(var,'*');
      txt_for=m2s(zeros(0,1));
      txt_for($+1,1)="int icase;";
      if size(condi,'*') == 1 &&  ~is_function_argument(condi.get_varname[]) then
	// loop is not usefull 
	txt_for($+1,1)=sprintf("icase= Max(1,Min(%d, (int) %s));",ncases,...
			       print_expr(condi));
      else
	txt_for($+1,1)=sprintf("icase= Max(1,Min(%d, (int) %s[%s]));",ncases,...
			       print_expr(condi),indi.get_varname[]);
      end
      txt_for($+1,1)="switch (icase) ";
      txt_for($+1,1)="  {";
      Ind2="   ";
      dec = m2s(zeros(0,1));
      for casei=1:ncases
	txt_for($+1,1)=Ind2+"case "+print_string(casei)+":"
	ini=rhs.exp(2)(casei+1)
	if type(ini,'short')<> 'bvar' then 
	  ini = numerics(ini);
	  dec.concatd[print_var('constant',ini)];
	end
	if prod(size(valueof(var)))==1 then
	  // we use print_expr who takes care of the fact that 
	  // function argument of size 1 are pointers 
	  txt_for($+1,1)=Ind2+"  "+print_expr(var)+"="+print_expr(ini)+ending()
	else
	  txt_for($+1,1)=Ind2+"  "+sprintf("%s[%s]=%s[%s];",var.get_varname[],indi.get_varname[],...
					   ini.get_varname[],indi.get_varname[]);
	end
	txt_for($+1,1)=Ind2+"  break;"
      end
      txt_for($+1,1)="  }"; // close the switch 
      indin = indi.get_varname[];
      if prod(size(valueof(var))) > 1 then
	txt($+1,1)=Ind+sprintf("for ( %s = 0; %s < %d ; %s++)",indin,indin,nz,indin);
	txt($+1,1)=Ind+"  {";
	Ind1=Ind+"   ";
	txt.concatd[Ind1+dec];
	txt.concatd[Ind1+txt_for];
	txt($+1,1)=Ind+"  }"; // close the for 
      else
	txt.concatd[Ind+dec];
	txt.concatd[Ind+txt_for];
      end
      // end of Select_exp 
    else
      // ?
      if prod(size(valueof(var)))>1 then 
	for kj=1:prod(size(valueof(var)))
	  cons=valueof(rhs);
	  txt($+1,1)=Ind+var.get_varname[]+"["+print_string(kj-1)+"]="+print_string(cons(kj)) +ending()
	end
      else
	// we have to check if var is a function argument or not 
	// since we generate an assignement var = rhs or var[0]=rhs
	// depending on var status 
	r=var.get_varname[];if  is_function_argument(r) then r = '*'+r;end
	rhsstr=print_expr(rhs);
	txt($+1,1)=Ind+r+"="+rhsstr +ending()
      end
    end
  endfunction

  // produce a C version of the code 
  
  delims=["[","]"];
  global(_lib_defs=hash(10));
  txt=m2s(zeros(0,1));
    
  Ind="  ";
  for m=1:length(code)
    elt=code(m);
    select elt(1) 
     case "nop" then
      // nothing to do 
     case "annotation" then
      // annotation 
      txt($+1,1)=Ind+"/* "+elt(2)+"*/"
     case "set" then
      // a set 
      var=elt(2);rhs=elt(3);
      txt1=print_set_statement(var,rhs);
      txt.concatd[txt1];
     case "bi_insert" then
      var=elt(2);i=elt(3);j=elt(4);rhs=elt(5)
      txt($+1,1)=Ind+pcode_extract3(var,i,j)+"="+print_expr(rhs)+ending()
     case "uni_insert" then
      var=elt(2);i=elt(3);rhs=elt(4)
      txt($+1,1)=Ind+pcode_extract2(var,i)+"="+print_expr(rhs)+ending()
     case "assign" then
      // assign 
      var=elt(2);rhs=elt(3);
      if size(var,'*') > 1 then ...
	// this can appear for first assignement of a non scalar variable
	rhsstr=print_expr(rhs)
	txt($+1,1)=Ind+var.get_varname[]+"[0]="+rhsstr+ending()
      else 
	// here the rhs can be a value or an If_expr or a 
	if type(rhs,'short') <> 'h' then rhs=expression("value",list(rhs));end
	txt1=print_set_statement(var,rhs);
	txt.concatd[txt1];
      end
     case "mcopy" then
      // mcopy 
      var=elt(2);in=elt(3);
      if type(in,'short')<>"bvar" then disp('wrong memcpy type'),pause, end
      in_name = in.get_varname[];
      ou_name= var.get_varname[];
      sz = size(var,'*');
      if sz == 1 then 
	if part(in_name,1)== '&' then 
	  in_name = part(in_name,2:length(in_name)); index=0;
	end;
	if is_function_argument(var.get_varname[]) then 
	  txt($+1,1)=Ind+sprintf("*%s=%s;", ou_name, in_name);
	else
	  txt($+1,1)=Ind+sprintf("%s=%s;", ou_name, in_name);
	end
      else
	txt($+1,1)=Ind+sprintf("memcpy(%s,%s,%d*%s);", ou_name, in_name,sz,...
			       sizeof(datatype(var)));
      end
     case "if_expr" then
      // if_expr 
      condi=elt(2)
      txt($+1,1)=Ind+"if ("+print_expr(condi)+") {"
      txt($+1,1)=Ind+"  "+print_expr(elt(3))
      if length(elt)==3 then
	txt($+1,1)=Ind+"}"
      else
	txt($+1,1)=Ind+"} else {"
	txt($+1,1)=Ind+"  "+print_expr(elt(4))
	txt($+1,1)=Ind+"}"
      end
     case "ident" then
      // 
      txt($+1,1)= print_expr(elt(2))+";"
     case "endfunction" then
      // postponed to the end of this file
     case "callf" then 
      // "callf" assumes all arguments are pointers (inouts)
      t=elt(2).exp
      fname=t(2)(1);outs=t(2)(2);ins=t(2)(3);
      s_outs=m2s([]);
      for e=outs ; 
	if type(e,'short')=="bvar" then
	  nm = e.get_varname[];
	  if size(valueof(e),'*')==1 then
	    if part(nm,1)=="*" then
	      s_outs.concatr[part(nm,2:length(nm))];
	    else
	      if part(nm,1:6)=="inouts" then
		s_outs.concatr[nm];
	      else
		s_outs.concatr['&'+nm];
	      end
	    end
	  else
	    s_outs.concatr[nm];
	  end
	else
	  s_outs.concatr[print_string(e)];
	end
      end
      s_ins=m2s([]);
      for e=ins ; 
	if type(e,'short')=="bvar" then
	  nm = e.get_varname[];
	  if size(valueof(e),'*')==1 then
	    if part(nm,1)=="*" then
	      s_ins.concatr[part(nm,2:length(nm))];
	    else
	      if part(nm,1:6)=="inouts" then
		s_ins.concatr[nm];
	      else
		s_ins.concatr['&'+nm];
	      end
	    end
	  else
	    s_ins.concatr[nm];
	  end
	else
	  s_ins.concatr[print_string(e)];
	end
      end; 
      txt($+1,1)= sprintf('%s(%s);',fname,catenate([s_outs,s_ins],sep=","));
      _lib_defs(fname)=%t;
      // argument is an expression
      // txt($+1,1)=print_expr(elt(2));
    else
      printf("unknown operation "+elt(1));
      pause 
    end
  end

  txt_dec=m2s(zeros(0,1));
  for t=declarations
    select t(1) 
     case "nop" then  
     case "annotation" then txt_dec.concatd["/* "+t(2)+" */"];
     case {'persistent','constant'} then
      if length(t)==3 && t(3)<>"" then 
	txt_dec($+1,1)="/* " + t(3) + "*/";
      end
      if type(t(2),'short') == 'bvar' then 
	txt_dec.concatd[print_var(t(1),t(2))];
      end
     case 'ephemere' then
      if length(t)==3 && t(3)<>"" then 
	txt_dec($+1,1)="/* " + t(3) + "*/";
      end
      // declaration of an ephemeral variable 
      // i.e declaration like type var ou type var[]
      var=t(2);
      typ=datatype(var);val=var.get_value[];sz=size(val);
      if ~var.is_symbolic[] then 
	// when an ephemeral is declared and is not symbolic 
	// we make as it is a constant XXXX 
	// this should be useless if ephemeral are properly declared.
	txt_dec.concatd[print_var('constant',t(2))];
      else
	typ=Ctypeof(typ)
	if prod(sz)==1 then
	  txt_dec($+1,1)=typ+" "+var.get_varname[]+";"
	else
	  txt_dec($+1,1)=typ+" "+var.get_varname[]+"["+sprintf("%d",prod(sz))+"]"+ending()
	end
      end
     case 'function' then
      // unused the tag endfunction do the job
      fname=t(2);io=t(3);
      args= io.values;//io.__keys;
      sargs="";
      for i=1:size(args,'*')-1
	elt = io(args(i));
	if size(elt.get_value[],'*') > 1 then
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
      txt_dec($+1,1)=""
      txt_dec($+1,1)="void "+fname+"("+sargs+"){"
    else
      error('sdfddsf')
    end
  end

  txt=[Ind+txt_dec;txt];
  
  // last pass to encapsulate the code in a function declaration if requested 
  Ind="  ";
  for m=1:length(code)
    elt=code(m);
    select elt(1) 
     case "endfunction" then
      // txt is to be enclosed in a function 
      // all arguments are assumed to be pointers 
      fname=elt(2);io=elt(3);
      args= io.values;//io.__keys;
      sargs=m2s([]);
      for i=1:size(args,'*')
	elt = io(args(i));
	// the name may have changed during evaluation 
	// the new name is no more args(i)
	sargs=[sargs,Ctypeof(datatype(elt))+" *"+args(i)];
      end
      txtn="";
      txtn($+1,1)="void "+fname+"("+catenate(sargs,sep=",")+"){"
      txtn.concatd[txt];
      // take care that names may have changed 
      // due to code evaluation 
      for i=1:size(args,'*')
      	newname = zins(i).get_varname[];
	if ~newname.equal[args(i)] then 
	  // insert args(i) = newname(i) 
	  nz=size(zins(i),'*');
	  if nz > 1 then 
	    if %t then 
	      txtn($+1,1)= Ind+sprintf("memcpy(%s,%s,%d*sizeof(%s));",args(i),newname,nz,Ctypeof(datatype(zins(i))));
	    else
	      for kj=0:nz-1
		txtn($+1,1)=Ind+sprintf("%s[%d]=%s[%d];",args(i),kj,newname,kj);
	      end
	    end
	  else
	    txtn($+1,1)=Ind+sprintf("%s[0]=%s;",args(i),newname);
	  end
	end
      end
      txtn.concatd["}"];      
      txt=txtn;
    end
  end

endfunction
