// Copyright 2013-2015 Enpc, Altair Engineering Inc
//
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
// Dealing with bvar objects
//
// some function are used to create bvar objects.
// some functions here are standard functions
//      which are here overloaded for bvar objects.
// a few functions which are defined for
// bvar and non bvar objects.
//
//
// Authors: J.-Ph. Chancelier (Cermics/ENPC), R. Nikoukhah (Altair Engineering)

//-----------------------------------------------
// creation of bvar objects
//-----------------------------------------------

function x=bvar(varname="",symbolic=%f,value=[])
// simplified way to create a bvar variable
// bvar(varname="poo",symbolic=%f,value=rand(4,5))
// Note: if value is already a bvar then its value is used
  x=bvar_create();
  x.set_varname[varname];
  x.set_symbolic[symbolic];
  if type(value,'short')=='bvar' then
    x.set_value[value.get_value[]];
  else
    x.set_value[value];
  end
endfunction

function out=symbolics(y,name)
// bvar(varname=name | getunique(), symbolic=%t,value=y);
  if nargin <= 1 then name=getunique();end
  out=bvar(symbolic=%t,value=y,varname=name);
endfunction

function out=numerics(y,name)
// bvar(varname=name | getunique(), symbolic=%f,value=y);
// XXXX: why numerics(string)->string ?
  if type(y,'short')=='s' then out=y;return;end
  if nargin <= 1 then name=getunique();end
  out=bvar(symbolic=%f,value=y,varname=name);
endfunction

//--------------------------
// unique identifiers
//----------------------------

function is=getunique(tag)
// return a new ``unique'' name
  global _i;  _i.add[1]
  if nargin < 1 then
    is="tmp_"+string(_i);
  else
    is=string(_i);
  end
endfunction

function getunique_reset()
// return a new ``unique'' name
  global _i;
  _i = 0;
endfunction

//-----------------------------------------------
// gen_def: generate a declaration and a set instruction
//-----------------------------------------------

function gen_def(var,rhs)
// var : a bvar variable.
// rhs : a code-expression.
// add a declaration of type 'ephemere' for var
// and add an instruction var=rhs in code if rhs is present
// a typical use of gen_def is
//  out=symbolics(...,getunique())
//  rhs=expression("+",list(in1,in2),overflow_opt)
//  gen_def(out,rhs)


  code_declaration_insert('ephemere',var);
  code_insert('set',var,rhs);
endfunction

function out=constant(y,name)
// creates a new bvar of numeric type and add an 'ephemere' declaration
// Note: we do not use gen_def since the set instruction is
// performed at declararion for numerics
  out=numerics(y,name);
  code_declaration_insert('ephemere',out);
  // code_insert('set',out,expression("value",list(out.get_value[])));
endfunction

//-----------------------------------------------
// code_declaration_insert():
//   insert instruction in declarations
//-----------------------------------------------

function code_declaration_insert(tag,var,msg="")
// var : a bvar variable.
// tag : 'persistent' 'constant' 'ephemere'
//       (static)     (const)    (standard)
  global(declarations=list());
  declarations($+1)=list(tag,var,msg)
endfunction

//-----------------------------------------------
// utility function
// which always return a bvar
//-----------------------------------------------

function var=Empty(in)
// creates a new symbolic variable similar to in
// and generates an 'ephemere' declaration
  var=bvar(varname=getunique(),value=valueof(in),symbolic=%t);
  code_declaration_insert('ephemere',var);
endfunction

function var=expand(in,m,n)
// creates and declares a new bvar variable
// which is an mxn matrix filled with in as value
// and with symbolic type from in
// used when in is 1x1
  select type(valueof(in),'short')
   case 'm' then   val=zeros(m,n);
   case 'b' then   val=bmat_create(m,n);
   case 's' then   val=smat_create(m,n);
   case 'i' then   v=valueof(in);val=m2i(zeros(m,n),v.itype[]);
  else
    val=zeros(m,n);
  end
  val(:,:)=valueof(in)
  if type(in,'short')=="bvar" then
     ty=in.is_symbolic[];
  else
     ty=%f;
  end
  var=bvar(varname=getunique(),value=val,symbolic=ty);
  code_declaration_insert('ephemere',var);
endfunction

//-----------------------------------------------
// functions defined for standard variables
// with an overloaded version for bvar
//-----------------------------------------------

function M=bvarcopy(Min)
// identity if Min is not a bvar
  M=Min
endfunction

function M=bvarcopy_bvar(Min)
// overloaded version for bvar
// performs M=Min and add declarations
// and copy code
  M=Min;
  M.set_varname[getunique()];
  code_declaration_insert('ephemere',M);
  if size(M,'*') ==1 then
    code_insert('set',M, expression("value",list(Min)));
  else
    code_insert("mcopy",M,Min);
  end
endfunction

function M=bvarempty(Min)
// identity if Min is not a bvar
  M=Min
endfunction

function M=bvarempty_bvar(Min)
// overloaded version for bvar
// creates a new bvar and add declarations
  M=Min;
  M.set_varname[getunique()];
  code_declaration_insert('ephemere',M);
endfunction

function rep=is_matrix_bvar(x)
  rep = is_matrix(x.get_value[]);
endfunction

function rep=is_matrix(x)
  if or(type(x,'short')==['m','i','b']) then
    rep=%t;
  else
    rep=%f
  end
endfunction

function out=is_sym(in)
  out = %f;
endfunction

function out=is_sym_bvar(in)
// overloaded version.
  out = in.is_symbolic[];
endfunction

function out=is_num(in)
  if or(type(in,'short')==['m','i']) then
    out = %t;
  else
    out = %f;
  end
endfunction

function out=is_num_bvar(in)
  out = %f;
endfunction

function out=valueof(in)
  out = in
endfunction

function out=valueof_bvar(in)
  out = in.get_value[];
endfunction

function out=datatype(in)
  out=type(in,'short')
  select out
   case 'm' then
    // scalar matrices
    if isreal(in) then
      out="double";
    else
      out="complex";
    end
   case 'i' then
    // integers
    out = in.itype[];
   case 'h' then
    // hash table
    out = in.type
  end
endfunction

function out=datatype_bvar(in)
// overloaded version for bvar
  out= datatype(in.get_value[]);
endfunction

function str=getvarname(x)
  str="unknown";
endfunction

function str=getvarname_bvar(x)
  str=x.get_varname[];
endfunction

//-------------------------------------
// existing library functions overloaded for bvar
//-------------------------------------

function [t,s]=size_bvar(x,mode)
// size(bvar);
// this function can be hardcoded in bvar.c
// it can be redefined here if requested
  if nargin==1  then
    t= size(x.get_value[]);
  else
    t= size(x.get_value[],mode);
  end
  if nargout==2 then [t,s]=(t(1),t(2)); end
endfunction

function r=isreal_bvar(in)
// overloaded isreal
  r=isreal(valueof(in))
endfunction

function out=abs_bvar(in)
// overload abs
  if ~is_sym(in) then out=(abs(valueof(in)));return;end
  sz = size(in);
  if and(sz == 1) then
    // we could keep and Abs here and generate a macro if Abs is used
    out=If_exp(in > (0),in,-in)
  else
    //out=Empty(abs(valueof(in)))
    out=bvarempty(in);
    for i=1:sz(1)
      for j=1:sz(2)
        out(i,j)=abs(in(i,j))
      end
    end
  end
endfunction

function out=sign_bvar(in)
// overload sign
  if ~is_sym(in) then out=(sign(valueof(in)));return;end
  sz = size(in);
  if and(sz == 1) then
    out2=If_exp(in < (0),(-1),(0));
    out=If_exp(in > (0),(1),out2);
  else
    // out=Empty(valueof(in))
    out=bvarempty(in);
    for i=1:sz(1)
      for j=1:sz(2)
        out(i,j)=sign(in(i,j))
      end
    end
  end
endfunction

function out = minus_bvar(in)
// unary minus
  global overflow_option
  if ~is_sym(in) then out= ( - valueof(in));return,end
  if prod(size(valueof(in)))==1 then
    out=symbolics(-valueof(in),getunique())
    rhs=expression("-",list(in),overflow_option)
    gen_def(out,rhs)
  else
    //out=Empty(valueof(in))
    out=bvarempty(in);
    sz=size(valueof(in))
    for i=1:sz(1)
      for j=1:sz(2)
        out(i,j)=-in(i,j)
      end
    end
  end
endfunction

// eye

function out=eye_bvar_bvar(in1,in2,varargopt)
  if varargopt.iskey['like'] && type(varargopt.like,'short')=='bvar' then
    varargopt.like=varargopt.like.get_value[];
  end
  if in1.is_symbolic[] || in2.is_symbolic[] then
    error("expecting non-symbolic arguments ");
    return
  end
  invalue=eye(in1.get_value[],in2.get_value[],varargopt(:));
  out=numerics(invalue);
  code_declaration_insert('constant',out,msg="eye");
endfunction

function out=eye_bvar(in,varargopt)
  if in.is_symbolic[] then
    error("expecting non-symbolic arguments ");
    return;
  end
  if varargopt.iskey['like'] && type(varargopt.like,'short')=='bvar' then
    varargopt.like=varargopt.like.get_value[];
  end
  invalue=eye(in.get_value[],varargopt(:));
  out=numerics(invalue);
  code_declaration_insert('constant',out,msg="eye");
endfunction

function out=eye_bvar_m(in1,in2,varargopt)
  out=eye_bvar_bvar(in1,numerics(in2),varargopt(:));
endfunction

function out=eye_bvar_i(in1,in2,varargopt)
  out=eye_bvar_bvar(in1,numerics(in2),varargopt(:));
endfunction

function out=eye_m_bvar(in1,in2,varargopt)
  out=eye_bvar_bvar(numerics(in1),in2,varargopt(:));
endfunction

function out=eye_i_bvar(in1,in2,varargopt)
  out=eye_bvar_bvar(numerics(i2m(in1)),in2,varargopt(:));
endfunction

// zeros

function out=zeros_bvar_bvar(in1,in2,varargopt)
  if varargopt.iskey['like'] && type(varargopt.like,'short')=='bvar' then
    varargopt.like=varargopt.like.get_value[];
  end
  if in1.is_symbolic[] || in2.is_symbolic[] then
    error("expecting non-symbolic arguments ");
    return
  end
  invalue=zeros(in1.get_value[],in2.get_value[],varargopt(:));
  out=numerics(invalue);
  code_declaration_insert('constant',out,msg="zeros");
endfunction

function out=zeros_bvar(in,varargopt)
  if in.is_symbolic[] then
    error("expecting non-symbolic arguments ");
    return;
  end
  if varargopt.iskey['like'] && type(varargopt.like,'short')=='bvar' then
    varargopt.like=varargopt.like.get_value[];
  end
  invalue=zeros(in.get_value[],varargopt(:));
  out=numerics(invalue);
  code_declaration_insert('constant',out,msg="zeros");
endfunction

function out=zeros_bvar_m(in1,in2,varargopt)
  out=zeros_bvar_bvar(in1,numerics(in2),varargopt(:));
endfunction

function out=zeros_bvar_i(in1,in2,varargopt)
  out=zeros_bvar_bvar(in1,numerics(in2),varargopt(:));
endfunction

function out=zeros_m_bvar(in1,in2,varargopt)
  out=zeros_bvar_bvar(numerics(in1),in2,varargopt(:));
endfunction

function out=zeros_i_bvar(in1,in2,varargopt)
  out=zeros_bvar_bvar(numerics(i2m(in1)),in2,varargopt(:));
endfunction

// ones

function out=ones_bvar_bvar(in1,in2,varargopt)
  if varargopt.iskey['like'] && type(varargopt.like,'short')=='bvar' then
    varargopt.like=varargopt.like.get_value[];
  end
  if in1.is_symbolic[] || in2.is_symbolic[] then
    error("expecting non-symbolic arguments ");
    return
  end
  invalue=ones(in1.get_value[],in2.get_value[],varargopt(:));
  out=numerics(invalue);
  code_declaration_insert('constant',out,msg="ones");
endfunction

function out=ones_bvar(in,varargopt)
  if in.is_symbolic[] then
    error("expecting non-symbolic arguments ");
    return;
  end
  if varargopt.iskey['like'] && type(varargopt.like,'short')=='bvar' then
    varargopt.like=varargopt.like.get_value[];
  end
  invalue=ones(in.get_value[],varargopt(:));
  out=numerics(invalue);
  code_declaration_insert('constant',out,msg="ones");
endfunction

function out=ones_bvar_m(in1,in2,varargopt)
  out=ones_bvar_bvar(in1,numerics(in2),varargopt(:));
endfunction

function out=ones_bvar_i(in1,in2,varargopt)
  out=ones_bvar_bvar(in1,numerics(in2),varargopt(:));
endfunction

function out=ones_m_bvar(in1,in2,varargopt)
  out=ones_bvar_bvar(numerics(in1),in2,varargopt(:));
endfunction

function out=ones_i_bvar(in1,in2,varargopt)
  out=ones_bvar_bvar(numerics(i2m(in1)),in2,varargopt(:));
endfunction

function out=iconvert_bvar(in,t)
// overloading of iconvert
// ?
  global overflow_option
  select t,
   case 0
    op=double;sop='double'
   case 1
    op=int8;sop='int8'
   case 11
    op=uint8;sop='uint8'
   case 2
    op=int16;sop='int16'
   case 12
    op=uint16;sop='uint16'
   case 4
    op=int32;sop='int32'
   case 14
    op=uint32;sop='uint32'
  else
    error('unknown type number '+string(t))
  end
  if ~is_sym(in) then out=(op(valueof(in))),return,end
  if prod(size(valueof(in)))==1 then
     out=symbolics(op(valueof(in)),getunique())
     rhs=expression(sop,list(in),overflow_option)
     gen_def(out,rhs)
  else
    // out=Empty(valueof(in))
    out=bvarempty(in);
    sz=size(valueof(in))
    for i=1:sz(1)
      for j=1:sz(2)
	out(i,j)=op(in(i,j))
      end
    end
  end
endfunction

function out=convert_bvar(in,ty)
// make a convertion and generate code
// for a bvar variable
// ty is assumed here to be a string.
  global overflow_option
  if datatype(in)==ty then out=in;return;end
  if is_sym(in) then
    val = in.get_value[];
    if size(val,'*') == 1 then
      out=symbolics(convert(val,ty),getunique())
      rhs=expression(ty,list(in,symbolics(datatype(in))),overflow_option)
      gen_def(out,rhs);
    else
      out=Empty(convert(val,ty));
      sz=size(val)
      for j=1:sz(2)
	for i=1:sz(1)
	  out(i,j)=convert(in(i,j),ty)
	end
      end
    end
  else
    out = in;
    out.set_value[convert(in.get_value[],ty)];
  end
endfunction

function out=convert_m(in,ty)
// convert data only
  global overflow_option
  if datatype(in)==ty then out=in; return;end
  select ty
   case "double" then out=real(in);
   case "complex" then out=complexify(in);
   case {"int8","uint8","int16","uint16","int32", "uint32"} then
    out=m2i(in,ty);
   case "b" then out= m2b(in);
  else
    error(ty+': data type not supported')
  end
endfunction

function out=convert_b(in,ty)
// convert data only
  global overflow_option
  if datatype(in)==ty then out=in;
  else
    out=convert(b2m(in),ty);
  end
endfunction

function out=convert_i(in,ty)
// convert data only
  global overflow_option
  if datatype(in)==ty then out=in;
  else
    // could be done
    out=convert(i2m(in),ty);
  end
endfunction

function out=concatr_bvar_bvar(in1,in2)
// %var_c_var(in1,in2) <-> [in1,in2]
  if datatype(in1) <> datatype(in2) then error("Incompatible types."),end
  if ~is_sym(in1) & ~is_sym(in2) then
    out=([valueof(in1),valueof(in2)]);
    return
  end
  code_insert("annotation",sprintf('Begin concatr of %s with %s',getvarname(in1),getvarname(in2)));
  out=Empty([valueof(in1),valueof(in2)])
  sz1=size(valueof(in1));sz2=size(valueof(in2));
  for i=1:sz1(1)
     for j=1:sz1(2)
       out(i,j)=in1(i,j)
     end
     for j=1:sz2(2)
       out(i,j+sz1(2))=in2(i,j)
     end
  end
  code_insert("annotation",sprintf('end concatr of %s with %s',getvarname(in1),getvarname(in2)));
endfunction

function out=concatr_bvar_m(in1,in2)
  out = concatr_bvar_bvar(in1 , in2);
endfunction

function out=concatr_m_bvar(in1,in2)
  out = concatr_bvar_bvar(in1, in2);
endfunction

function out=concatd_bvar_bvar(in1,in2)
// %var_f_var(in1,in2)
// [in1; in2]
  if datatype(in1) <> datatype(in2) then error("Incompatible types."),end
  if ~is_sym(in1) & ~is_sym(in2) then
    out=([valueof(in1);valueof(in2)]);
    return
  end
  out=Empty([valueof(in1);valueof(in2)])
  sz1=size(valueof(in1));sz2=size(valueof(in2));
  for j=1:sz1(2)
     for i=1:sz1(1)
       out(i,j)=in1(i,j)
     end
     for i=1:sz2(1)
       out(i+sz1(1),j)=in2(i,j)
     end
  end
endfunction

function out=concatd_bvar_m(in1,in2)
  out = concatd_bvar_bvar(in1 , in2);
endfunction

function out=concatd_m_bvar(in1,in2)
  out = concatd_bvar_bvar(in1, in2);
endfunction

function out= mult_bvar_bvar(in1,in2)
// out= %var_m_var(in1,in2)
// in1 * in2
  global overflow_option
  maxsizeprod=6;
  if datatype(in1) <> datatype(in2) then
    if datatype(in1)=="b" & datatype(in2)=="double" then
      in1=convert(in1,"double")
    elseif datatype(in2)=="b" & datatype(in1)=="double" then
      in2=convert(in2,"double")
    else
      error("Incompatible types.")
    end
  end
  if ~is_sym(in1) & ~is_sym(in2) then
    out=(valueof(in1)*valueof(in2));
    return;
  end
  if prod(size(valueof(in1)))==1 & prod(size(valueof(in2)))==1 then
    if is_sym(in1) | is_sym(in2) then
      if (~is_sym(in1) & valueof(in1).equal[0] ) |(~is_sym(in2) & valueof(in2).equal[0]) then
	out=0; // (valueof(in1)*valueof(in2)); // result will be zero
      elseif ~is_sym(in1) & valueof(in1)==1 then
         out=in2;return
      elseif  ~is_sym(in2) & valueof(in2)==1 then
         out=in1;return
      else
	out=symbolics(valueof(in1)*valueof(in2),getunique())
	rhs=expression("*",list(in1,in2),overflow_option)
	gen_def(out,rhs)
      end
    end
  elseif prod(size(valueof(in1)))==1 then
    out=Empty(valueof(in2))
    sz=size(valueof(in2));
    for i=1:sz(1)
      for j=1:sz(2)
        out(i,j)=in1*in2(i,j)
      end
    end
  elseif prod(size(valueof(in2)))==1 then
    out=Empty(valueof(in1))
    sz=size(valueof(in1));
    for i=1:sz(1)
      for j=1:sz(2)
        out(i,j)=in1(i,j)*in2
      end
    end
  else
    sz1=size(valueof(in1));sz2=size(valueof(in2))
    m1=sz1(1);m2=sz2(1);n1=sz1(2);n2=sz2(2);
    if m1*n2> maxsizeprod & datatype(in1)=="double" then
      // generate a call ZZZ
      code_insert("annotation",'Product of matrices resulting size '+..
		     string(m1*n2)+'>'+string(maxsizeprod)+': calling external function')
      //Z= convert(zeros(size(valueof(in1),1),size(valueof(in2),2)),datatype(in1));
      out=Empty(valueof(in1)*valueof(in2))
      call_func("mult",list(out,in1,in2,m1,n1,m2,n2));
      out.set_value[valueof(in1)*valueof(in2)];
    else
      out=Empty(valueof(in1)*valueof(in2))
      for i=1:m1
	for j=1:n2
	  sd=convert((0),datatype(in1))
	  for k=1:n1
	    sd=sd+in1(i,k)*in2(k,j)
	  end
	  out(i,j)=sd
	end
      end
    end
  end
endfunction


function out=mult_bvar_m(in1,in2)
  out = mult_bvar_bvar(in1 , convert(in2,datatype(in1)));
endfunction

function out=mult_m_bvar(in1,in2)
  out = mult_bvar_bvar(convert(in1,datatype(in2)) , in2);
endfunction

function out=mult_bvar_i(in1,in2)
  out = mult_bvar_bvar(in1 , in2);
endfunction

function out=mult_i_bvar(in1,in2)
  out = mult_bvar_bvar(in1, in2);
endfunction

function out=div_bvar_bvar(in1,in2)
// out= %var_r_var(in1,in2)
// in1 / in2
  if datatype(in1) <> datatype(in2) then error("Incompatible types."),end
  if ~is_sym(in1) & ~is_sym(in2) then
    out=(valueof(in1)/valueof(in2));
    return
  end
  if size(valueof(in2),'*') >1 then
    out=in1*inv(in2);
    return;
  end
  if size(valueof(in1),'*')==1 then
    if is_sym(in1) | is_sym(in2) then
      if (~is_sym(in1)&valueof(in1)==0) then
         out=0 // out=valueof(in1)/valueof(in2)
      elseif (~is_sym(in2)&valueof(in2)==0) then
         error("Singularity encountered in division.")
      else
         vin2=valueof(in2)
         if vin2==0 then vin2=vin2+1;end
         out=symbolics(valueof(in1)/vin2,getunique())
         rhs=expression("/",list(in1,in2))
         gen_def(out,rhs)
      end
    end
  else
    out=Empty(valueof(in1))
    sz=size(valueof(in1))
    for i=1:sz(1)
      for j=1:sz(2)
        out(i,j)=in1(i,j)/in2
      end
    end
  end
endfunction

function out=div_bvar_m(in1,in2)
  out = div_bvar_bvar(in1 , convert(in2,datatype(in1)));
endfunction

function out=div_m_bvar(in1,in2)
  out = div_bvar_bvar(convert(in1,datatype(in2)) , in2);
endfunction

function out=div_i_m(in1,in2)
  out = div_bvar_bvar(in1 , convert(in2,datatype(in1)));
endfunction

function out=div_m_i(in1,in2)
  out = div_bvar_bvar(convert(in1,datatype(in2)) , in2);
endfunction

function out=div_bvar_i(in1,in2)
  out = div_bvar_bvar(in1 , in2);
endfunction

function out=div_i_bvar(in1,in2)
  out = div_bvar_bvar(in1, in2);
endfunction


function out=bdiv_bvar_bvar(in1,in2)
// in1 \ in2
  if datatype(in1) <> datatype(in2) then error("Incompatible types."),end
  if ~is_sym(in1) & ~is_sym(in2) then
    out=(valueof(in1)\valueof(in2));
    return
  end
  if size(valueof(in1),'*') >1 then
    out=inv(in1)*in2;
    return;
  end
  if size(valueof(in2),'*')==1 then
    if is_sym(in1) | is_sym(in2) then
      if (~is_sym(in2)&valueof(in2)==0) then
         out=0
      elseif (~is_sym(in1)&valueof(in1)==0) then
         error("Singularity encountered in division.")
      else
         vin1=valueof(in1)
         if vin1==0 then vin1=vin1+1;end
         out=symbolics(vin1\valueof(in2),getunique())
         rhs=expression("\",list(in1,in2))
         gen_def(out,rhs)
      end
    end
  else
    out=Empty(valueof(in2))
    sz=size(valueof(in2))
    for i=1:sz(1)
      for j=1:sz(2)
        out(i,j)=in1\in2(i,j)
      end
    end
  end
endfunction

function out=bdiv_bvar_m(in1,in2)
  out = bdiv_bvar_bvar(in1 , convert(in2,datatype(in1)));
endfunction

function out=bdiv_m_bvar(in1,in2)
  out = bdiv_bvar_bvar(convert(in1,datatype(in2)) , in2);
endfunction

function out=bdiv_i_m(in1,in2)
  out = bdiv_bvar_bvar(in1 , convert(in2,datatype(in1)));
endfunction

function out=bdiv_m_i(in1,in2)
  out = bdiv_bvar_bvar(convert(in1,datatype(in2)) , in2);
endfunction

function out=bdiv_bvar_i(in1,in2)
  out = bdiv_bvar_bvar(in1 , in2);
endfunction

function out=bdiv_i_bvar(in1,in2)
  out = bdiv_bvar_bvar(in1, in2);
endfunction

function out=extractelts_bvar(u,i)
// extraction out=u(i);
// out=u(i);
  if ~is_sym(u) & ~is_sym(i) then
    iv=valueof(i);uv=valueof(u);
    out=(uv(iv));
    return
  end
  if prod(size(valueof(i)))==1 then
    // extract one value
    uv=valueof(u);
    // i =valueof(i);
    // here extraction cannot be performed if i is bvar but it's not 
    // important since we are producing a symbolic variable
    if type(i,'short')=='bvar' then z=1; else z= valueof(i);end 
    out=symbolics(uv(z),getunique());
    //    i= bvar(varname=getunique(),value=convert(i,"int32"),symbolic=%f);
    i=convert(i,"int32")
    if prod(size(valueof(u)))==1 then
      // rhs=u;
      rhs=expression("value",list(u));
    else
      rhs=expression("u_extract",list(u,i))
    end
    gen_def(out,rhs);
  else
    // extract a submatrix
    iv=valueof(i);
    uv=valueof(u);
    out=Empty(uv(iv));
    szi=size(iv);
    code_insert("annotation",sprintf('Begin extraction from %s of size %dx%d',u.get_varname[],szi(1),szi(2)));
    k=1
    for ix=1:szi(1)
      for jx=1:szi(2)
	out(k)=u(i(ix,jx)); // row in nsp
	k=k+1
      end
    end
    code_insert("annotation",sprintf('End of extraction of %s, result in"+...
				     " %s',u.get_varname[],out.get_varname[]));
  end
endfunction

function out=extractcols_bvar(u,i)
// extraction of columns out=u(:,i);
  out=extract_bvar(u,1:size(valueof(u),1),i);
endfunction

function out=extractrows_bvar(u,i)
// extraction of columns out=u(:,i);
  out=extract_bvar(u,i,1:size(valueof(u),2));
endfunction

function out=extract_bvar(u,i,j)
// extraction out=u(i,j);
  if nargin==2 then
    out=extractelts_bvar(u,i);
  elseif nargin==3 then
    // u(i,j)
    if ~is_sym(i) & ~is_sym(j) & ~is_sym(u) then
      iv=valueof(i);jv=valueof(j);uv=valueof(u)
      out=(uv(iv,jv));
      return
    end
    if prod(size(valueof(i)))==1 & prod(size(valueof(j)))==1 then
      uv=valueof(u)
      out=symbolics(uv(valueof(i),valueof(j)),getunique());
      //      i= bvar(varname=getunique(),value=convert(valueof(i),"int32"),symbolic=%f);
      i=convert(i,"int32")
      //      j= bvar(varname=getunique(),value=convert(valueof(j),"int32"),symbolic=%f);
      j=convert(j,"int32")
      if prod(size(uv))==1 then
	// rhs=u;
	rhs=expression("value",list(u));
      else
	rhs=expression("b_extract",list(u,i,j))
      end
      gen_def(out,rhs)
    else
     iv=ones(size(valueof(i)))
     jv=ones(size(valueof(j)))
     uv=valueof(u)
     out=Empty(uv(iv,jv))
     code_insert("annotation",sprintf('Begin of extraction u(i,j) of %s, result in %s',...
				      u.get_varname[],out.get_varname[]));
     szi=size(iv);
     szj=size(jv)
     for ix=1:prod(szi)
       for jx=1:prod(szj)
         out(ix,jx)=u(i(ix),j(jx))
       end
     end
     code_insert("annotation",sprintf('End of extraction u(i,j) of %s, result in %s',...
				      u.get_varname[],out.get_varname[]));

    end
  else
    error('zefr')
  end
endfunction

function out=setrowscols_bvar(lval,i,j,val)
// insertion lval(i,j)=val
//  %var_i_var(i,j,in,Min) Min(i;j)=in
  if nargin==4 then
    if "double"== datatype(val) then
      val=convert(val,datatype(lval))
    elseif datatype(lval)<>datatype(val) then
      error("incompatible data types.")
    end
    if type(i,'short')== 'iv' then
      // when i is :
      i=1:size(valueof(lval),'r');
    end
    if type(j,'short')== 'iv' then
      // when j is :
      j=1:size(valueof(lval),'c');
    end
    if type(lval,'short')<>"bvar" & ~is_sym(val) & ~is_sym(i) & ~is_sym(j) then
      out=valueof(lval);invalue=valueof(val);iv=valueof(i);jv=valueof(j)
      out(iv,jv)=invalue;
      return
    end
    szM=size(valueof(lval))
    //experimental
    if and(szM==size(valueof(val))) then out=copyvar(val);return;end
    M= copyvar(lval);
    if prod(size(valueof(i)))==1 & prod(size(valueof(j)))==1 then
      out=M;
      if ~out.is_symbolic[] then
	// if out was not symbolic it inherits the val symbolic property
	out.set_symbolic[ is_sym(val) |is_sym(i)|is_sym(j) ];
      end
      //      i= bvar(varname=getunique(),value=convert(valueof(i),"int32"),symbolic=%f);
      i=convert(i,"int32")
      //      j= bvar(varname=getunique(),value=convert(valueof(j),"int32"),symbolic=%f);
      j=convert(j,"int32")
      outval=out.get_value[];
      outval(valueof(i),valueof(j))=valueof(val)
      out.set_value[outval];
      //  out.name=getunique()
      if prod(size(valueof(out)))==1 then
        code_insert("assign",out,val)
      else
        code_insert("bi_insert",out,i,j,val)
      end
    else
      szi=prod(size(valueof(i)));szj=prod(size(valueof(j)))
      out=M; // Empty(M)
      code_insert("annotation",sprintf('Begin setrowscols(i,j) of %s with %s',out.get_varname[],getvarname(val)));
      if size(valueof(val),'*') == 1 then
      	for ix=1:szi
	  for jx=1:szj
	    out(i(ix),j(jx))=val;
	  end
	end
      else
	for ix=1:szi
	  for jx=1:szj
	    out(i(ix),j(jx))=val(ix,jx)
	  end
	end
      end
      code_insert("annotation",sprintf('End setrowscols of %s',out.get_varname[]));
    end
  elseif nargin==3 then
    // lval(i)=j
    if "double"== datatype(j) then
      j=convert(j,datatype(lval))
    elseif datatype(lval)<>datatype(j) then
      error("incompatible data types.")
    end
    if type(i,'short')== 'iv' then
      // when i is :
      i=1:size(valueof(lval),'*');
    end
    if  type(lval,'short')<>"bvar" & ~is_sym(i) & ~is_sym(j) then
      out=valueof(lval);invalue=valueof(j);iv=valueof(i);
      out(iv)=invalue
      return;
    end
    //    M= lval; // copyvar(lval);
    M= copyvar(lval);
    val=j;
    if prod(size(valueof(i)))==1 then
      out=M;
      //      i= bvar(varname=getunique(),value=convert(valueof(i),"int32"),symbolic=%f);
      i=convert(i,"int32")
      outval = out.get_value[];
      outval(valueof(i))=valueof(val);
      out.set_value[outval];
      if ~out.is_symbolic[] then
	// if out was not symbolic it inherits the val symbolic property
	out.set_symbolic[ is_sym(val) |is_sym(i)];
      end
      if prod(size(valueof(M)))==1 then
	code_insert("assign",out,val)
      else
	code_insert('uni_insert',out,i,val);
      end
    else
      szi=prod(size(valueof(i)));
      out=M;
      if type(val,'short')=="bvar"  then
	valname = val.get_varname[];
      else
	valname = "...";
      end
      code_insert("annotation",sprintf('Begin of set %s(1:%d)=%s',out.get_varname[],szi,valname));
      if prod(size(valueof(val))) == 1 then
	for ix=1:szi
	  out(i(ix))=val;
	end
      else
	for ix=1:szi
	  out(i(ix))=val(ix);
	end
      end
      code_insert("annotation",sprintf('End of set result in %s',out.get_varname[]));
    end
  else
    error('oiojo')
  end
endfunction

function out=quote_bvar(in)
// transposition
  if ~is_sym(in) then out=((valueof(in))');return;end
  maxsizetr= 6;
  tr_in=valueof(in)
  m=size(tr_in,1);
  n=size(tr_in,2);
  if m*n==1 then
    out=in
  else
    //
    out=Empty(tr_in');
    if m*n>maxsizetr & datatype(out)=="double" then
      code_insert("annotation",'Transpose of matrix of size '+string(m*n)+'>'+string(maxsizetr)+': calling external function')
      call_func("quote",list(out,in,m,n))
      code_insert("annotation",'End of Transpose')
    else
      for i=1:m
        for j=1:n
          out(j,i)=in(i,j)
        end
      end
    end
  end
endfunction

function out=resize2vect_bvar(u)
// u(:)
  out=extractelts_bvar(u,1:size(valueof(u),'*'));
endfunction

function out=tozero_bvar(u)
//u(:)=[]
// pas clair !
  out=extractelts_bvar(u,([]));
endfunction

function out=deleteelts_bvar(u,elts)
//u(elts)=[]
  out = resize2vect_bvar(u);
  I= 1:size(valueof(u),"*");
  I(valueof(elts))=[]
  out=extractelts_bvar(u,I);
endfunction

function out=deleterows_bvar(u,rows)
//u(elts)=[]
  I= 1:size(valueof(u),"r");
  I(valueof(rows))=[];
  out=extract_bvar(u,I,1:size(valueof(u),'c'));
endfunction

function out=deletecols_bvar(u,cols)
//u(elts)=[]
  I= 1:size(valueof(u),"c");
  I(valueof(cols))=[];
  out=extract_bvar(u,1:size(valueof(u),'r'),I);
endfunction

function out=impl_bvar(min,step,max)
//
  if nargin == 2 then
    max= step;
    if datatype(min) <> datatype(max) then error("Incompatible types."),end
    if ~is_sym(min) & ~is_sym(max) then
      out=(valueof(min):valueof(max));
      return;
    else
      error("Implicit vector should not be symbolics");
    end
  else
    if datatype(min) <> datatype(max) && ...
	  datatype(step) <> datatype(max) then
      error("Incompatible data types."),
    end;
    if ~is_sym(min) & ~is_sym(max) & ~is_sym(step) then
      out=(valueof(min):valueof(step):valueof(max));
      return;
    else
      error("Implicit vector should not be symbolics");
    end
  end
endfunction

function out=hat_bvar_bvar(M,p)
  p = valueof(p);
  if floor(p)~=p then error("Only integer powers of matrices supported.");end
  if ~M.is_symbolic[] then
    out= M.get_value[]^p;return;
  end
  if p==0 then out=eye(size(M));return;end
  if p < 0 then
    error("Negative powers of matrices not implemented yet.");
    M=inv(M);p=-p;
  end
  if p==1 then out=M;return;end
  oddflag = %f;
  while p>1
    if modulo(p,2)==1 then
      if ~oddflag then
	oddflag=%t;
	oddmat=M;
      else
	oddmat=M*oddmat
      end
    end
    M=M*M;
    p=floor(p/2);
  end
  if oddflag then
    M=M*oddmat
  end
  out=M
endfunction

function out=hat_bvar_m(M,p)
  out=hat_bvar_bvar(M,p);
endfunction

function out=hat_m_bvar(M,p)
  out=hat_bvar_bvar(M,p);
endfunction

function out=hat_bvar_i(M,p)
 out=hat_bvar_bvar(M,p)
endfunction

function out=hat_m_i(M,p)
 out=M^double(p);
endfunction

function out=hat_i_m(M,p)
 out=M^convert(p,datatype(M));
endfunction

function out=hash_bvar(n)
// maybe not required
  out=hash(n.get_value[]);
endfunction

function out=string_bvar(in)
  out=string(in.get_value[]);
endfunction

// matrix invert

function out=atan_bvar_m(in1,in2)
  out =atan_bvar_bvar(in1 , in2);
endfunction

function out=atan_m_bvar(in1,in2)
  out =atan_bvar_bvar(in1 , in2);
endfunction

function out=atan_bvar_bvar(in1,in2)
  if ~is_sym(in1) & ~is_sym(in2) then
    out=(atan(valueof(in1),valueof(in2)));return;
  end
  if size(in1,'*') == size(in2,'*') then
    if size(in1,'*') == 1 then
      out=symbolics(atan(valueof(in1),valueof(in2)),getunique())
      rhs=expression("atan",list(in1,in2))
      gen_def(out,rhs);
    else
      out=Empty(valueof(in1))
      sz=size(in1)
      for i=1:sz(1)
	for j=1:sz(2)
	  out(i,j)=atan(in1(i,j),in2(i,j))
	end
      end
    end
  elseif prod(size(in2))==1 then
    out=Empty(valueof(in1))
    sz=size(in1)
    for i=1:sz(1)
      for j=1:sz(2)
        out(i,j)=atan(in1(i,j),in2)
      end
    end
  elseif prod(size(in1))==1 then
    out=Empty(valueof(in2))
    sz=size(in2)
    for i=1:sz(1)
      for j=1:sz(2)
        out(i,j)=atan(in1,in2(i,j))
      end
    end
  elseif and(size(in1)==size(in2)) then
    out=Empty(valueof(in2))
    sz=size(in2)
    for i=1:sz(1)
      for j=1:sz(2)
        out(i,j)=atan(in1(i,j),in2(i,j))
      end
    end
  else
    error("atan arguments have incompatible sizes.")
  end
endfunction

function out=inv_bvar(in)
  if ~is_sym(in) then out=inv(valueof(in));return;end  
  [m,n]=size(in)
  if m<>n then error('Inverting a non square matrix not allowed.");end
  if datatype(in)<>"double" then error("Only double matrices can be inverted.");end
  if m==1 then 
    out=1/in;
  elseif m==2 then
    out=bvarempty(in)
    out(1,1)=in(2,2)
    out(2,2)=in(1,1)
    out(1,2)=-in(1,2)
    out(2,1)=-in(2,1)
    out=out/(in(1,1)*in(2,2)-in(1,2)*in(2,1))
  else
    out=copyvar(in)
    ipiv=Empty(convert(zeros(1,m),"int32"))
    info=Empty(convert(0,"int32"))
    tmpwork=Empty(zeros(m))
    m=convert(m,"int32")
    call_func("C2F(dgetrf)",list(m,m,out,m,ipiv,info));
    call_func("C2F(dgetri)",list(m,out,m,ipiv,tmpwork,m,info));
    out.set_value[valueof(in)]  
  end
endfunction
