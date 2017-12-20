// this script will generate a set of overloaded 
// functions for bvar variables 

f=fopen('bvar_lib.sci',mode="w");

f.put_smatrix[["// Warning: this file is automatically generated";
	       "// do not edit manually !";"";
	       "function version=bvar_lib() version=""1.0"";endfunction";""]];

// elts-wise operations
elts=["+"   ,"-"    ,".*" ,"./"  ,".\"  ,"&"  ,"|" ,"&&"     ,"||"    
      "plus","minus","dst", "dsl", "dbs","and","or","seq_and","seq_or"];

elts=[elts,["==","<>" ,">"  ,"<" ,">=","<=",".^"
	    "eq","ne","gt" ,"lt","ge","le","dh"]]; 

function txt=op_def(op)
  if op(1)=="+" then
     txt_op=["    if ~is_sym(in1)&valueof(in1)==0 then out=in2;return;end"
             "    if ~is_sym(in2)&valueof(in2)==0 then out=in1;return;end"]
  elseif op(1)=="-" then
     txt_op=["    if ~is_sym(in2)&valueof(in2)==0 then out=in1;return;end"]
     txt_op=["    if ~is_sym(in1)&valueof(in1)==0 then out=-in2;return;end"]
  elseif or(op(1)==["&","&&"]) then
     txt_op=["    if ~is_sym(in1)&valueof(in1) then out=in2;return;end"
             "    if ~is_sym(in2)&valueof(in2) then out=in1;return;end"
             "    if ~is_sym(in1)&~valueof(in1) then out=0;return;end"
             "    if ~is_sym(in2)&~valueof(in2) then out=0;return;end"]
  elseif or(op(1)==["|","||"]) then
     txt_op=["    if ~is_sym(in1)&valueof(in1) then out=1;return;end"
             "    if ~is_sym(in2)&valueof(in2) then out=1;return;end"
             "    if ~is_sym(in1)&~valueof(in1) then out=in2;return;end"
             "    if ~is_sym(in2)&~valueof(in2) then out=in1;return;end"]
  elseif op(1)==".*" then
     txt_op=["    out=in1*in2;return"]
  elseif op(1)=="./" then
     txt_op=["    out=in1/in2;return"]
  elseif op(1)==".\" then
     txt_op=["    out=in2/in1;return"]
  else
     txt_op=[]
  end

  txt=["function out="+op(2)+"_bvar_bvar(in1,in2)"
       "  global overflow_option"
       "  overflow_opt=overflow_option"
       "  if isempty(overflow_opt) then overflow_opt='"overflow'";end"
       "  if datatype(in1) <> datatype(in2) then error(""Incompatible types""),end"
       "  if ~is_sym(in1) && ~is_sym(in2) then "
       "    out=(valueof(in1)"+op(1)+"valueof(in2));"
       "    return"
       "  end"
       "  if (prod(size(valueof(in1)))==1) & (prod(size(valueof(in2)))==1) then"
       txt_op
       "    vin1= valueof(in1);vin2= valueof(in2);"
       "    out=symbolics(vin1"+op(1)+"vin2,getunique())"
       "    rhs=expression("""+op(1)+""",list(in1,in2),overflow_opt)"
       "    gen_def(out,rhs)"
       "  else"
       "    out=Empty(valueof(in1)"+op(1)+"valueof(in2))"
       "    sz=size(valueof(out));sz1=size(valueof(in1));sz2=size(valueof(in2))"
       "    for i=1:sz(1)"
       "      for j=1:sz(2)"
       "        out(i,j)=in1(min(i,sz1(1)),min(j,sz1(2)))"+op(1)+"in2(min(i,sz2(1)),min(j,sz2(2)))"
       "      end"
       "    end"
       "  end"
       "endfunction"
       ""
       "function out="+op(2)+"_bvar_m(in1,in2)"
       "  out="+op(2)+"_bvar_bvar(in1,convert(in2,datatype(in1)))"
       "endfunction"
       ""
       "function out="+op(2)+"_m_bvar(in1,in2)"
       "  out="+op(2)+"_bvar_bvar(convert(in1,datatype(in2)),in2)"
       "endfunction"
       ""
       "function out="+op(2)+"_bvar_i(in1,in2)"
       "  out="+op(2)+"_bvar_bvar(in1,in2)"
       "endfunction"
       ""
       "function out="+op(2)+"_i_bvar(in1,in2)"
       "  out="+op(2)+"_bvar_bvar(in1,in2)"
       "endfunction"
       ""
       "function out="+op(2)+"_i_m(in1,in2)"
       "  out="+op(2)+"_bvar_bvar(in1,convert(in2,datatype(in1)))"
       "endfunction"
       ""
       "function out="+op(2)+"_m_i(in1,in2)"
       "  out="+op(2)+"_bvar_bvar(convert(in1,datatype(in2)),in2)"
       "endfunction"
       ""];
endfunction

for op=elts
  txt=op_def(op);
  f.put_smatrix[txt];
end

//basic functions
fcts=["sin","cos","tan","asin","acos","atan","sinh","cosh","tanh","sqrt","log","exp","floor","ceil","int"];

function txt=fct_def(fct)
  txt=["function out="+fct+"_bvar(in)"
       "  if ~is_sym(in) then out=("+fct+"(valueof(in)));return;end"
       "  if (prod(size(valueof(in)))==1) then"
       "    out=symbolics("+fct+"(valueof(in)),getunique())"
       "    rhs=expression("""+fct+""",list(in))"
       "    gen_def(out,rhs)"
       "  else"
       "    out=Empty("+fct+"(valueof(in)))"
       "    sz=size(valueof(out));"
       "    code_insert(''annotation'',sprintf(''"+fct+"(%s) of size %dx%d'',in.get_varname[],sz(1),sz(2)));"
       "    for i=1:sz(1)"
       "      for j=1:sz(2)"
       "        out(i,j)="+fct+"(in(i,j))"
       "      end"
       "    end"
       "    code_insert(''annotation'',sprintf(''end of "+fct+"(%s), result in %s'',in.get_varname[],out.get_varname[]));"
       "  end"
       "endfunction"
       ""];
endfunction

for fct=fcts
  txt = fct_def(fct);
  f.put_smatrix[txt];
end

// XXXX: Attention comportement different entre 
// scicoslab et nsp 

gops=["prod","sum","and","or"
      "*", "+"  ,"&"  ,"|"];

function txt=gop_def(gop)
  txt=["function out="+gop(1)+"_bvar(in,varagin)"
       "  if nargin==1 then"
       "    if ~is_sym(in) then out=("+gop(1)+"(valueof(in)));return,end"
       "    out=in(1,1)"
       "    sz=size(valueof(in))"
       "      for i=1:sz(1)"
       "        for j=1:sz(2)"
       "          if ~((i==1)&(j==1)) then  out=out"+gop(2)+"in(i,j);end"
       "        end"
       "      end"
       "  elseif nargin==2 then"
       "    in2=varargin(1)"
       "    out=Empty(valueof(in),valueof(in2))"
       "    sz1=size(valueof(in));sz2=size(valueof(in2));"
       "    sz=max(sz1,sz2)"
       "      for i=1:sz(1)"
       "        for j=1:sz(2)"
       "          out(i,j)=in(i,j)"+gop(2)+"in2(i,j);"
       "        end"
       "      end"
       "  else"
       "    out=in"
       "      for i=1:nargin-1"
       "        out=out"+gop(2)+"varagin(i)"
       "      end"
       "  end"
       "endfunction"
      ""]
endfunction

for gop=gops
  txt = gop_def(gop);
  f.put_smatrix[txt];
end

// math int operations
// -------------------

elts=["+"   ,"-"    ,".*" ,".^", "*",  "/",  "./"  ,".\" ;
      "plus","minus","dst","dh", "mult","div","dsl", "dbs"];

function txt=int_op_def(op)
  txt=["function out="+op(2)+"_i_i(in1,in2)"
       "  global overflow_option"
       "  overflow_opt=overflow_option"
       "  if isempty(overflow_opt) then overflow_opt='"overflow'";end"
       "  if datatype(in1)<>datatype(in2) then error('"Incompatible data types.'");end"
       "    if overflow_opt=='"overflow'" then"
       "      s=over_flow(i2m(in1)"+op(1)+"i2m(in2),datatype(in1))"
       "    elseif  overflow_opt==""satur"" then"
       "      s=saturate(i2m(in1)"+op(1)+"i2m(in2),datatype(in1))"
       "    else"
       "      error('"Overflow option '"+overflow_opt+'" unknown.'")"
       "  end"
       "  out=convert(s,datatype(in1))"
       "endfunction"
       ""
       "//function out=void_"+op(2)+"_m_i(in1,in2)"
       "//  out=convert(in1,datatype(in2))"+op(1)+"in2"
       "//endfunction"
       ""
       "//function out=void_"+op(2)+"_i_m(in1,in2)"
       "//  out=in1"+op(1)+"convert(in2,datatype(in1))"
       "//endfunction"
       ""];
endfunction

for op=elts
  txt=int_op_def(op);
  f.put_smatrix[txt];
end


// comparison int operations

elts=["==","<>" ,">"  ,"<" ,">=","<=";
      "eq","ne","gt" ,"lt","ge","le"];

function txt=comp_int_op_def(op)
  txt=["function out="+op(2)+"_i_i(in1,in2)";
       "  out=i2m(in1)"+op(1)+"i2m(in2)";
       "endfunction";
       "";
       "function out="+op(2)+"_m_i(in1,in2)";
       "  out=(in1"+op(1)+"i2m(in2))";
       "endfunction";
       "";
       "function out="+op(2)+"_i_m(in1,in2)";
       "  out=(i2m(in1)"+op(1)+"in2)";
       "endfunction";
       ""];
endfunction

for op=elts
  txt=comp_int_op_def(op);
  f.put_smatrix[txt];
end

function txt=deffints(ty)
  txt=["function y=_NAME_m(u)";
       "  y=m2i(u,""NAME"");"
       "endfunction"
       ""
       "function y=_NAME_i(u)";
       "  y=u.retype[""NAME""];"
       "endfunction"
       ""
       "function y=_NAME_bvar(u)";
       "  y=convert(u,""NAME"");"
       "  if type(y,''short'') <> ''bvar'' then "
       "    y=bvar(varname=getunique(),value=y,symbolic=%f);"
       "  end";
       "endfunction"];
  txt=strsubst(txt,"NAME",ty);
endfunction

for ty=["int8","uint8","int16","uint16","int32","uint32"]
  txt = deffints(ty);
  f.put_smatrix[txt];
end


function txt=deflogics(op)
   txt=["function y="+op+"_m(u1,u2)"
        "  if nargin==1 then y="+op+"(m2b(u1)) else y="+op+"(m2b(u1),m2b(u2));end"
        "endfunction"
        ""
        "function y="+op+"_i(u1,u2)"
        "  if nargin==1 then y="+op+"_m(i2m(u1)) else y="+op+"_m(i2m(u1),i2m(u2));end"
        "endfunction"];
endfunction

for op=["or","and"]
  txt=deflogics(op)
  f.put_smatrix[txt];
end

f.close[];



