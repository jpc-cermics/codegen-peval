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

function txt=code_printer_nsp(code,declarations)
// 
  delims=["(",")"];

  function r = print_expr(rhs)
  // print an expression 
    global _defs
    tps=["double","complex","_int8","_uint8","_int16","_uint16",...
	 "_int32","_uint32","boolean","b","int8","uint8","int16",...
	 "uint16","int32","uint32"];
    opt = type(rhs,'short');
    if opt == 'h' then opt = rhs.type; end
    if or(opt==tps) then
      r=string(valueof(rhs))
    elseif or(opt == ["m","i","b"]) then
      r=string(rhs);
    elseif opt=="bvar" then
      // This should be the only case.
      if rhs.is_symbolic[] then 
	r=rhs.get_varname[];
      else
	// sans doute à revoir XXX 
	r=string(rhs.get_value[]);
      end
    elseif opt=="op" then
      t=rhs.exp
      if t(1)=="b_extract" then
	u=t(2)(1);i=t(2)(2);j=t(2)(3)
	r=sprintf("(%s)",pcode_extract3(u,i,j,delims=delims));
      elseif t(1)=="u_extract" then
	// Attention le i qui est ici peut-etre une expression 
	u=t(2)(1);i=t(2)(2);
	r=pcode_extract2(u,i,delims=delims); r="("+r+")"
      elseif or(t(1)==["==","<=",">=","<>","<",">","/"]) then
	in1=t(2)(1);in2=t(2)(2);
	r=print_expr(in1)+t(1)+print_expr(in2); r="("+r+")"
      elseif or(t(1)==["+"]) then
	in1=t(2)(1);in2=t(2)(2);
	if datatype(in1)=="double" | t(3).equal["overflow"] then
	  r=print_expr(in1)+t(1)+print_expr(in2); r="("+r+")"
	else
	  r="add"+datatype(in1)+"_"+t(3)+"("+print_expr(in1)+","+print_expr(in2)+")";
	  _defs=unique([_defs;"add"+datatype(in1)+"_"+t(3)])
	end
      elseif or(t(1)==["*"]) then
	in1=t(2)(1);in2=t(2)(2);
	if datatype(in1)=="double" | ~t(3).equal["satur"] then
	  r=print_expr(in1)+t(1)+print_expr(in2); r="("+r+")"
	else
	  r="mult"+datatype(in1)+"_"+t(3)+"("+print_expr(in1)+","+print_expr(in2)+")";
	  _defs=unique([_defs;"mult"+datatype(in1)+"_"+t(3)])
	end
      elseif t(1)=="-" then
	ins=t(2);
	if length(ins)==1 then
	  in1=ins(1)
	  if datatype(in1)=="double" | ~t(3).equal["satur"] then
	    r="-"+print_expr(in1); r="("+r+")"
	  else
	    r="neg"+datatype(in1)+"_"+t(3)+"("+print_expr(in1)+","+print_expr(in2)+")";r="("+r+")"
	    _defs=unique([_defs;"neg"+datatype(in1)+"_"+t(3)])
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
	  // atan can be of arity 2 atan2
	  r=t(1)+"("+print_expr(in1)+","+print_expr(t(2)(2))+")";
	end
      elseif or(t(1)==["int8","uint8","int16","uint16","int32","uint32"]) then
	in1=t(2)(1);typ = t(2)(2).get_value[]; r= print_expr(in1);
	// XXX: should take care here of overflow  in t(3)
	r=print_expr(in1);
      	select typ 
	 case "double" then r=sprintf("m2i(%s,""%s"")",r,t(1));
	 case {"int8","uint8","int16","uint16","int32","uint32"} then 
	  r=sprintf("i2i(%s,""%s"");",r,t(1));
	 case "b" then r=sprintf("m2i(b2m(%s),""%s"");",r,t(1));
	else
	  printf("wrong datatype for convert\n");pause ;
	end
      elseif or(t(1)==["boolean","b"]) then
	// convert to boolean from typ
	in1 = t(2)(1); typ = t(2)(2).get_value[]; r= print_expr(in1);
	select typ 
	 case "double" then r=sprintf("m2b(%s)",r);
	 case {"int8","uint8","int16","uint16","int32","uint32"} then 
	  r=sprintf("m2b(i2m(%s));",r);
	 case "b" then r=r;
	else
	  printf("wrong datatype for convert\n");pause ;
	end
      elseif t(1)=="double" then
	in1=t(2)(1)
	r="("+t(1)+") ("+print_expr(in1)+")"; //r="("+r+")" 
      elseif or(t(1)==["^",".^"]) then
	in1=t(2)(1);in2=t(2)(2);
	r="("+print_expr(in1)+"^"+print_expr(in2)+")";
      elseif t(1)=="call" then
	// list("call,list(fname,outs,ins))
	// special cases 
	if t(2)(1)== "quote" then 
	  fname=t(2)(1);
	  outs=t(2)(3)(1) // first elts in ins 
	  ins =t(2)(3)(2) // second elt in ins 
	  r = sprintf('%s=%s(%s);', outs.get_varname[],fname, ins.get_varname[]);
	elseif t(2)(1)== "mult" then 
	  fname=t(2)(1);
	  outs=t(2)(3)(1) // first elts in ins 
	  ins1 =t(2)(3)(2) // second elt in ins 
	  ins2 =t(2)(3)(3) // third elt in ins 
	  r = sprintf('%s=%s*%s;',outs.get_varname[],ins1.get_varname[],ins2.get_varname[]);
	else
	  fname=t(2)(1);outs=t(2)(2);ins=t(2)(3);
	  s_outs=m2s([]);for e=outs ; s_outs.concatr[e.get_varname[]];end;
	  s_ins=m2s([]);for e=ins ; s_ins.concatr[e.get_varname[]];end; 
	  if length(outs)==0 then 
	    r = sprintf('%s(%s);',fname, catenate(s_ins,sep=","));
	  else
	    r = sprintf('[%s]=%s(%s);', catenate(s_outs,sep=","),fname, ...
			catenate(s_ins,sep=","));
	  end
	end
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

  function txt=pcode_extract2(var,i,delims=["(",")"])
  // var(i) 
  // Note that i can be an expression or a bvar 
    idx=print_expr(i);
    txt=var.get_varname[]+delims(1)+idx+delims(2);
  endfunction
  
  function txt=pcode_extract3(var,i,j,delims=["(",")"])
  // extraction var(i,j)
  // take care of the fact that i and j can be bvar, numerics or expressions
    sz=size(valueof(var));
    if is_num(i) & is_num(j) then
      i=valueof(i);j=valueof(j);
      idx=string((j-1)*sz(1)+i)
    elseif ~is_num(i) & is_num(j) then
      j=valueof(j)
      idx=print_expr(i)+"+"+string((j-1)*sz(1))
    elseif is_num(i) & ~is_num(j) then
      i=valueof(i)
      idx=string(i)+"+"+string(sz(1))+"*("+print_expr(j)+"-1)"
    else
      idx=sprintf("%s+%s*(%s-1)",print_expr(i),string(sz(1)),print_expr(j));
    end
    txt=sprintf("%s%s%s%s",var.get_varname[],delims(1),idx,delims(2));
  endfunction

  function y=ending()
    y=";"
  endfunction


  function funlist=unused_fun_sat_defs( )

    function ty=Ctypeof(dt)
      ee=["int8","uint8","int16","uint16","int32","uint32","double","complex","boolean","b"]
      ef=["int8_t","uint8_t","int16_t","uint16_t","int32_t","uint32_t","double","double","gboolean","gboolean"]
      k=find(dt==ee(1,:))
      if isempty(k) then error('Unsupported data type: '+dt);end
      ty=ef(k)
    endfunction;
    
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

  // produce a nsp version of the code 
  // instead of C-code.
  txt=m2s([])
  // ephemeral which are not symbolic are treated like constants.

  function txt=print_var(vartype,var)
  // declaration of a constant or persistent variable 
    typ=datatype(var);sz=size(valueof(var));val=valueof(var);
    val=val(:);name=var.get_varname[];
    txt=m2s([]);
    if prod(sz)==1 then
      if type(val,'short')== 'i' then 
	txt($+1,1)=sprintf("%s=m2i(%s,""%s"");",name,string(val),typ);
      else
	txt($+1,1)=sprintf("%s=%s;",name,string(val));
      end
    else
      //save current format
      [save_field,save_prec,save_eflag]=format("get"); 
      format("long");
      values = strsubst(strsubst(sprint(val',as_read=%t),"[",""),"]","");
      format(save_field,save_prec,save_eflag);
      values = catenate(values,sep="\n");
      if type(val,'short')== 'i' then 
	txt($+1,1)=sprintf("%s=m2i([%s],""%s"";",name,values,typ);
      else
	txt($+1,1)=sprintf("%s=[%s];",name,values);
      end;
      if sz(1)<>1 then 
	txt($+1,1)=sprintf("%s.redim[%d,%d];",name,sz(1),sz(2));
      end
    end
  endfunction
  
  for t=declarations
    select t(1) 
     case "nop" then 
     case "annotation" then txt.concatd["// "+t(2)];
     case {'persistent','constant'} then
      if type(t(2),'short') == 'bvar' then 
	txt.concatd[print_var(t(1),t(2))];
      end
     case 'ephemere' then
      // declaration of an ephemeral variable 
      var=t(2);
      typ=datatype(var);val=var.get_value[];sz=size(val);
      if ~var.is_symbolic[] then 
	// when an ephemeral is declared and is not symbolic 
	// we make as it is a constant XXXX 
	// this should be useless if ephemeral are properly declared.
	txt.concatd[print_var('constant',t(2))];
      else
	if prod(sz) <> 1 then
	  select type(var.get_value[],'short'); 
	   case 'm' then 
	    txt($+1,1)=sprintf("%s=ones(%d,%d);// ephemeral",var.get_varname[],sz(1),sz(2));
	   case 'i' then 
	    txt($+1,1)=sprintf("%s=m2i(ones(%d,%d),""%s"");// ephemeral",var.get_varname[],sz(1),sz(2),val.itype[]);
	   case 'b' then 
	    txt($+1,1)=sprintf("%s=m2b(ones(%d,%d));// ephemeral",var.get_varname[],sz(1),sz(2));
	  else
	    printf("value has a wrong type\n");pause;
	  end
	end
      end
     case 'function' then
      // the job is done by endfunction 
    else
      error('sdfddsf')
    end
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
      txt($+1,1)=Ind+"// "+elt(2)
     case "set" then
      // a set 
      var=elt(2);rhs=elt(3) 
      if rhs.exp(1)=="value" then 
	if prod(size(valueof(var)))>1 then 
	  for kj=1:prod(size(valueof(var)))
	    cons=valueof(rhs.exp(2)(1));
	    txt($+1,1)=Ind+var.get_varname[]+"("+string(kj)+")="+string(cons(kj)) +ending()
	  end
	else
	  // 
	  rhsstr=print_expr(rhs.exp(2)(1))
	  txt($+1,1)=Ind+var.get_varname[]+"="+rhsstr +ending()
	end
      elseif rhs.exp(1)=="If_exp"
	condi=rhs.exp(2)(1);in1=rhs.exp(2)(2);in2=rhs.exp(2)(3);
	if prod(size(valueof(in1)))==1 then
	  txt($+1,1)=Ind+"if ("+print_expr(condi)+") then"
	  txt($+1,1)=Ind+"  "+var.get_varname[]+"="+print_expr(in1)+ending()
	  txt($+1,1)=Ind+"else "
	  txt($+1,1)=Ind+"  "+var.get_varname[]+"="+print_expr(in2)+ending()
	  txt($+1,1)=Ind+"end"
	else
	  txt($+1,1)=Ind+"if ("+print_expr(condi)+") then"
	  if type(in1,'short')<> 'bvar' then 
	    in1 = numerics(in1);
	    txt.concatd[print_var('constant',in1)];
	  end
	  txt($+1,1)=Ind+  var.get_varname[]+"="+in1.get_varname[]+ending();
	  txt($+1,1)=Ind+"else"
	  if type(in2,'short')<> 'bvar' then 
	    in2 = numerics(in2);
	    txt.concatd[print_var('constant',in2)];
	  end
	  txt($+1,1)=Ind+  var.get_varname[]+"="+in2.get_varname[]+ending();
	  txt($+1,1)=Ind+"end"
	end
      elseif rhs.exp(1)=="Select_exp"
	ncases=length(rhs.exp(2))-1
	condi=rhs.exp(2)(1);
	txt($+1,1)=Ind+"cond=max(1,min("+string(ncases)+","+print_expr(condi)+"));";
	for casei=1:ncases
	  // txt($+1,1)=Ind+" case "+string(casei)+":"
	  ini=rhs.exp(2)(casei+1);
	  if type(ini,'short')<> 'bvar' then 
	    ini = numerics(ini);
	    txt.concatd[print_var('constant',ini)];
	  end
	  if casei==1 then 
	    if prod(size(valueof(ini)))==1 then
	      txt($+1,1)=Ind+"  "+var.get_varname[]+"="+print_expr(ini)+ending()
	    else
	      txt($+1,1)=Ind+"  "+var.get_varname[]+"="+ini.get_varname[]+";";
	    end
	  else
	    txt($+1,1)=Ind+"condi=cond=="+string(casei)+ending();
	    if prod(size(valueof(ini)))==1 then
	      txt($+1,1)=Ind+"  "+var.get_varname[]+"(condi)="+print_expr(ini)+"(condi)"+ending()
	    else
	      txt($+1,1)=Ind+"  "+var.get_varname[]+"(condi)="+ini.get_varname[]+"(condi)"+ending()
	    end
	  end
	  //txt($+1,1)=Ind+"break;"
	end
	//txt($+1,1)=Ind+"}"
      else
	// ? 
	if prod(size(valueof(var)))>1 then 
	  for kj=1:prod(size(valueof(var)))
	    cons=valueof(rhs);
	    txt($+1,1)=Ind+var.get_varname[]+"("+string(kj)+")="+string(cons(kj)) +ending()
	  end
	else
	  // 
	  rhsstr=print_expr(rhs)
	  txt($+1,1)=Ind+var.get_varname[]+"="+rhsstr +ending()
	end
      end
     case "bi_insert" then
      var=elt(2);i=elt(3);j=elt(4);rhs=elt(5)
      txt($+1,1)=Ind+pcode_extract3(var,i,j,delims=delims)+"="+print_expr(rhs)+ending()
     case "uni_insert" then
      var=elt(2);i=elt(3);rhs=elt(4)
      txt($+1,1)=Ind+pcode_extract2(var,i,delims=delims)+"="+print_expr(rhs)+ending()
     case "assign" then
      // assign 
      var=elt(2);rhs=elt(3)
      if prod(size(valueof(var)))>1 then 
	// this can appear for first assignement of a non scalar variable
	rhsstr=print_expr(rhs)
	txt($+1,1)=Ind+var.get_varname[]+"(1)="+rhsstr+ending()
      else 
	rhsstr=print_expr(rhs)
	txt($+1,1)=Ind+var.get_varname[]+"="+rhsstr+ending()
      end
    case "mcopy" then
      // mcopy 
      var=elt(2);in=elt(3);
      if type(in,'short')<>"bvar" then disp('wrong memcpy type'),pause, end
      in_name = in.get_varname[];
      if part(in_name,1)== '&' then in_name = part(in_name,2:length(in_name));end
      txt($+1,1)=Ind+var.get_varname[]+"="+in_name+ending();
    case "if_expr" then
      // if_expr 
      condi=elt(2)
      txt($+1,1)=Ind+"if ("+print_expr(condi)+") then"
      txt($+1,1)=Ind+"  "+print_expr(elt(3))
      if length(elt)==3 then
	txt($+1,1)=Ind+"end"
      else
	txt($+1,1)=Ind+" else "
	txt($+1,1)=Ind+"  "+print_expr(elt(4))
	txt($+1,1)=Ind+"end"
      end
     case "endfunction" then
      fname=elt(2);io=elt(3);
      args= catenate(io.values,sep=",");
      txt=[sprintf("function [%s]=%s(%s)",args,fname,args);txt;"endfunction"];
     case "callf" then 
      // argument is an expression
      txt($+1,1)=print_expr(elt(2));
    else
      printf("unknown operation '+elt(1));
      pause 
    end
  end
endfunction

