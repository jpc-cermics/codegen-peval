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

function version=code_utils_library()
  version="1.0"
endfunction

function gencode_init()
  global overflow_option; overflow_option="overflow";
  global code;code=list();
  global declarations;declarations=list();
endfunction 

// ------------------------
// dealing with persistent 
// (i.e C static variables)
// ------------------------

function out=persistent_create()
// utilities to gather persitent declarations
  out= hash(10);
endfunction

function out=persistent_extract(A,name)
// get the value of a persistent variable 
  DateCode=acquire('DateCode',def='');
  if type(name,'short')<>"s" then name="z_"+DateCode+string(name);end 
  if A.iskey[name] then
    out = A(name);
  else
    error("Error:"+name+" does not exist as a persistent variable");
  end
endfunction

function A=persistent_insert(A,name,in)
// insert a persistent variable named name in structure A
// in: value to insert in the persistent variable 
// name: name of the persistent variable 
//       name can be a string or an integer 
//       or a bvar containing a string or an integer
// 
  DateCode=acquire('DateCode',def='');
  if type(name,'short')== 'bvar' then name=name.get_value[];end
  if type(name,'short')<>"s" then name="z_"+DateCode+string(name);end 
  if A.iskey[name] then 
    szin=size(valueof(in))
    if ~and(size(valueof(A(name)))==szin) then
      error ('incompatible sizes'),
    end
    if prod(szin)==1 then
      code_insert("assign",A(name),in);
    else
      mcopy(A(name),in);
    end
  else
    var = symbolics(in,name);
    code_declaration_insert('persistent',var);
    A(name) = var;
  end
endfunction

// dealing with inouts: used for function generation 
// -------------------------------------------------

function out=inouts()
  out=hash(10);
  out.values=m2s([]);// keep track of order 
endfunction

function A=inouts_insert(A,name,in)
// insert name in A using name as tag 
// Note: here name is a string 
  if A.iskey[name] then 
    szin=size(valueof(in))
    if ~and(size(valueof(A(name)))==szin) then
      error ('incompatible sizes'),
    end
    if prod(szin)==1 then
      code_insert("assign",A(name),in);
    else
      mcopy(A(name),in);
    end
  else
    A(name) = symbolics(in,name);
    A.values[$+1,1]=name;
  end
endfunction

// suite 

function out=inputs(num,val,name)
   out=symbolics(val,name)
   gen_inputs(num,out)
endfunction

function gen_inputs(num,var)
   global argins
   argins(num)=list(valueof(var),var.get_varname[])
endfunction

function out=outputs(num,val,name)
   out=symbolics(val,name)
   gen_outputs(num,out)
endfunction

function gen_outputs(num,var)
   global argouts
   argouts(num)=list(valueof(var),var.get_varname[])
endfunction

// initialize global variables used in code generation.

function codegen_init()
// initialize
  global Used_variables;Used_variables= hash(10);
  global top_declarations;top_declarations=list();
  global text;text=m2s([]);
  global code;code=list();
  global declarations;declarations=list();
//  global filename;filename=path  
endfunction

function textout=codegen_finalize(filename=m2s([]),target_code=m2s([]), keep_all=%f)
// generate code in textout 
// if filename is given the code is also writen in filename 
// target_code can be given to specify the target code "C" "P" or "nsp"
// if target_code is not given then we use
// target=acquire('target',def="C");
    
  function L=RemoveUnused(L,Used_variables)
  // Used_variables is a Hash table 
  // L is a list of declarations;
    del=[];
    Id=1:length(L);
    for kk=Id;
      td=L(kk);
      if td(1)<>"function" && td(1)<>"nop" && td(1) <> "annotation" && type(td(2),'short')== 'bvar' then
	varname=td(2).get_varname[]
	if ~(Used_variables.iskey[varname]) then del=[del,kk];end
      end
    end;
    Id(del)=[];
    L= L.sublist[Id];
  endfunction
    
  function [code,declarations]=gencode_initialize(top_declarations)
  // utility function used for creation of a initialize function
    code = list();
    declarations=list();
    zero=numerics(0);
    zero_exists=%f;
    zeroi=numerics(0i);
    zeroi_exists=%f;
    for i=1:length(top_declarations)
      elt=top_declarations(i);
      if elt(1)=='persistent' then 
	in=elt(2);
	var=numerics(in);
	if zero.equal[var] then 
	  // try to keep a unique declaration for zero;
	  if zero_exists == %f then 
	    code_declaration_insert('persistent',zero);
	    declarations($+1)=list('persistent',zero);
	    zero_exists = %t;
	  end;
	  code($+1)=list('mcopy',in,zero);
	elseif zeroi.equal[var] then 
	  // try to keep a unique declaration for zeroi;
	  if zeroi_exists == %f then 
	    code_declaration_insert('persistent',zeroi);
	    declarations($+1)=list('persistent',zeroi);
	    zeroi_exists = %t;
	  end;
	  code($+1)=list('mcopy',in,zeroi);
	else
	  code_declaration_insert('persistent',var);
	  code($+1)=list('mcopy',in,var);
	  declarations($+1)=list('persistent',var);
	end
      end
    end
    DateCode=acquire('DateCode',def='');
    init_fname= "initialize"+DateCode;
    code($+1)=list("endfunction",init_fname,hash(values=m2s([])));
  endfunction
    
  global text;
  global(top_declarations=list());
  global(Used_variables=hash(10));
  
  target= target_code;
  if isempty(target) then target=acquire('target',def="C");end
  
  if keep_all then 
    // no optimize 
    opt = [%f,%f,%f];
  end
  
  if ~keep_all then 
    top_declarations=RemoveUnused(top_declarations,Used_variables);
  end
  
  // insert a function to re-initialize the static variables
  [code1,declarations1]=gencode_initialize(top_declarations)
  Lres2=list();zins=list();is_in=[];
  select target 
   case "C" then 
    text_init=code_printer_c(code1,declarations1);
   case "P" then 
    text_init=code_printer_xmi(code1,declarations1,keeprefs=%t);
   case "nsp" then 
    text_init=code_printer_nsp(code1,declarations1);
  end
  
  // global code for top_declarations 
  select target 
   case "C" then 
    [txtdec]=code_printer_c(list(),top_declarations);
   case "P" then 
    [textcode,txtdec]=code_printer_xmi(list(),top_declarations,keeprefs=%t);
   case "nsp" then 
    [txtdec]=code_printer_nsp(list(),top_declarations);
  end
  // full text 
  text=[txtdec;text_init;text];

  select target 
   case "P" then 
    // encapsulate in a module then in a GACodeModel
    [open_mod,close_mod]=codemodel_modules(name="nsp",header="nsp",id=getunique(1));
    text=[open_mod;"  "+text;close_mod];
    // insert the library functions declaration 
    // we need here to add the calledBy="" attribute 
    funlist=code_printer_xmi_sat_defs( );
    text_lib=m2s(zeros(0,1));
    for fun=_defs'
      if funlist.iskey[fun] then 
	text_lib.concatd[funlist(fun)];
      end
    end
    // we can add an external module here 
    //[open_mod,close_mod]=codemodel_modules(name="nsp1",header="nsp1",id=getunique(1),external="false");
    //text =[text;open_mod;"  "+text_lib;close_mod];
    [open_xml,close_xml]=codemodel_GACodeModel();
    text=[open_xml;"  "+text;close_xml];
  end
    
  if ~isempty(filename) then putfile(filename,text);end 
  textout=text;
  clearglobal text,top_declarations,Used_variables;
endfunction

// used to generate code for a function 

function StartFunction(f, fargs)
  global(declarations=list());
  global code,top_declarations;
  top_declarations.concat[declarations];
  declarations=list();
  code=list();
endfunction

function EndFunction(f, fargs)
// generates a 'enfunction' 
// which contains the same arguments as the corresponding 'function' 
  global text,code, top_declarations, declarations;
  global(Used_variables=hash(10));
  code($+1)=list("endfunction",f, fargs);
  opt=acquire('opt','callers',def=[%t,%t,%f]);
  // fargs changed to Lres2 and zins 
  // because printers use this information 
  Lres2=list();
  args= fargs.values;
  for i=1:size(args,'*')
    Lres2(i) = fargs(args(i));
  end
  [Lres2,code,declarations]=code_optimize(Lres2,code,declarations,top_declarations,opt=opt,verbose=%f,check=%f);
  zins=list();
  for i=1:length(Lres2);
    zins(i)=Lres2(i);
    is_in(i)=i;
  end
  // Hash table of used variables 
  usedvars=bvar_code_vars(code);
  Used_variables.merge[usedvars];
  //[code,declarations]=fix_outs(code,declarations)
  target=acquire('target',def="C");
  select target 
   case "C" then 
    [textcode]=code_printer_c(code,declarations);
   case "P" then 
    [textcode]=code_printer_xmi(code,declarations,keeprefs=%t);
   case "nsp" then 
    [textcode]=code_printer_nsp(code,declarations);
  end
  text.concatd[textcode];
  declarations=list();
  code=list();
endfunction

//-----------------------------------
// To be clarified 
//-----------------------------------

function res=is_op(t,op)
  if type(t,'short')=="h" && t.type == "op" then
    res = t.exp(1) == op;
  else
    res = %f;
  end
endfunction  

function mcopy(var,rhs)
// insert an instruction of type code_insert("mcopy",var,rhs);
// which performs var = rhs (by memcpy).
// Thus, if rhs is not a bvar a constant is created for rhs 
// before inserting
  
  if type(var,'short') <> "bvar" then 
    error("Error: expecting a symbolic arguments");
  end
  if type(rhs,'short') <> "bvar" then 
    if %f then 
      // XXXX: generates a constant declaration + a set 
      rhs=constant(rhs,getunique());
    else
      // just generate a constant declaration 
      rhs = numerics(rhs);
      code_declaration_insert('constant',rhs);
    end
  end
  code_insert("mcopy",var,rhs);
  if or( size(var)<>size(rhs)) then 
    error("Error: mcopy arguments should share the same size");
  end
endfunction

function M=copyvar(Min)
// utility 
  if type(Min,'short') == "bvar" then
    Min=Min;
  else
    Min=numerics(Min);
  end
  M=Min;
  M.set_varname[getunique()];
  if prod(size(valueof(M)))==1 then
    gen_def(M,expression("value",list(Min)));
  else
    if is_sym(M) then
      code_declaration_insert('ephemere',M)
      mcopy(M,Min)
    else
      rhs=Min
      constant(valueof(rhs),M.get_varname[]) 
    end
  end
endfunction

function if_cos(in,f1,f2)
// f1 and f2 are expressions of type call(..)
  if is_sym(in) then 
    code_insert("if_expr", in > (0),f1,f2);
  elseif valueof(in)>0 then
    code_insert("ident",f1)   
  else
    code_insert("ident",f2)   
  end
endfunction

function select_cos(in,varargin)
// 
  if is_sym(in) then 
    code_insert("switch_expr",in > (0),varargin(:));
  else
    code_insert("ident",varargin(valueof(in)));// call_func(varargin(valueof(in)))
  end
endfunction

function out=If_exp(condi,varargin)
// If_exp can be used with bvar or with 
// standard variables.
// varargin is of length 1 or two 
// 
  nin=length(varargin)
  if is_op(varargin(1),"call") then 
    // varargin is composed of call expressions 
    if nin==1 | (nin==2 & is_op(varargin(2),"call")) then
      if is_sym(condi) then
	// 
        code_insert("if_expr",condi,varargin);
      else
        if condi then
           f1=varargin(1)
           fname=f1(1)
           f1(1)=null()
           call_func(fname,f1)
        elseif ~condi & nin==2 then
           f2=varargin(2)
           fname=f2(1)
           f2(1)=null()
           call_func(fname,f2)
        end
      end
    else
      error("then and else branches are incompatible.\n")
    end
    return
  end  
  
  // the Scicos if block and usage of If_exp to call functions done
  // moving to standard If expression as expressional if with an output.
    
  if nin<>2 then error("Expressional if must have both then and else values."),end
  in1=varargin(1)
  in2=varargin(2)
  // Testing the size of the arguments
  [mv,nv]=size(valueof(in1).*valueof(in2).*double(valueof(condi))) //generate error in case of wrong sizes

  if ~is_sym(condi) then
    vcond=valueof(condi)
    if prod(size(vcond))==1 then
      if vcond then 
        if prod(size(in1))==1 then out=in1*ones(mv,nv); else out=in1; end
      else 
        if prod(size(in2))==1 then out=in2*ones(mv,nv); else out=in2; end
      end
    else
      out = zeros(mv,nv); //out=zeros(size(vcond));
      out=convert(out,datatype(in1))
      for i=1:prod(size(vcond))
        if vcond(i) then
      	  out(i)=in1(min(prod(size(valueof(in1))),i));
        else
          out(i)=in2(min(prod(size(valueof(in2))),i));
        end
      end
    end
  else
    // cond is symbolic 
    if ~size(valueof(in1)).equal[size(valueof(in2))] then 
      s1= size(valueof(in1));s2=size(valueof(in2));
      error(sprintf("incompatible sizes [%d,%d] and [%d,%d]",s1(1),s1(2),s2(1),s2(2)));
    end 
    if ~(datatype(in1) == datatype(in2)) then 
      error("incompatible types:" +datatype(in1)+" vs "+datatype(in1));
    end
    if prod(size(valueof(condi)))==1 then
      Z=convert(zeros(mv,nv),datatype(in1))
      out=symbolics(Z,getunique());
      rhs=expression("If_exp",list(condi,in1,in2)) ;
      gen_def(out,rhs);
    else
      // we should check here that condi is of proper size 
      sz=size(valueof(condi));
      out=Empty(in1+in2);  // sum to fix size in case one is scalar
      sz1=size(valueof(in1));
      sz2=size(valueof(in2));
      for i=1:sz(1)
        for j=1:sz(2)
          out(i,j)=If_exp(condi(min(i,sz(1)),min(j,sz(2))),in1(min(i,sz1(1)),min(j,sz1(2))),...
			  in2(min(i,sz2(1)),min(j,sz2(2)))); 
        end
      end
    end
  end
endfunction

function out=Select_exp(condi,varargin)
  nin=length(varargin)
  if is_op(varargin(1),"call") then
    for i =2:nin
      if ~is_op(varargin(i),"call") then
        error("Incomptible select cases.")
      end   
      if is_sym(condi) then
	code_insert("switch_expr",condi,varargin(:));
      else
        f1=varargin( valueof(condi))
        fname=f1(1)
        f1(1)=null()
        call_func(fname,f1)
      end
    end
    return
  else
    // here varagin is a list of matrices 
    for i=1:nin
      if ~is_matrix(varargin(i)) then error("only matrix arguments allowed in exp select");end
    end
    for i=1:nin-1
      if ~size(valueof(varargin(i))).equal[size(valueof(varargin(i+1)))] then 
        error("incompatible sizes:")
      end
      if ~(datatype(varargin(i))==datatype(varargin(i+1))) then 
        error("incompatible types");
      end 
    end
    if ~is_sym(condi) then
      // explicit value in non symbolic case 
      condi=min(nin,max(1,condi));
      out=varargin(1);
      for i=2:nin; cond=(condi==i); out(cond)=varargin(i)(cond);end 
    else
      out=symbolics(valueof(varargin(1)),getunique())
      rhs=expression("Select_exp",list(condi,varargin(:))) 
      gen_def(out,rhs)
    end
  end
endfunction

function rhs=expression(varargin)
  rhs=tlist(["op","exp"],varargin);
endfunction

function out=CallFunction(f,operands)
// returns an expression of type "call".
// Operands here can a hash table like the ones 
// created by inouts (i.e a hash table with a special entry .values 
//  which contains names  in requested order.
// typeof or type ? XXX
  if typeof(operands,'short')=='h' then 
    names = operands.values;
    args = list();
    for i=1:size(names,'*')
      args(i) = operands(names(i));
      if type(args(i),'short')<> "bvar" then
	args(i)=constant(args(i),getunique());
      end
    end
  else
    pause in CallFunction
  end
  out=expression("call",list(f,list(),args));
endfunction

function call_func(fname,args)
// returns a code tag
   for i=1:length(args)
     if type(args(i),'short')<> "bvar" then
       args(i)=constant(args(i),getunique());
     end
   end
   code_insert("callf",expression("call",list(fname,list(),args)));
endfunction

function out=saturate(in,ty)
  minout = intmin(ty);
  maxout = intmax(ty);
  out=min(i2m(maxout),max(in,i2m(minout)))
endfunction

function out=over_flow(in,ty)
  if ty=="int8" then
    out=mod(in,2^8);if out>2^7-1 then out=out-2^8;end
  elseif ty=="uint8" then
    out=mod(in,2^8);
  elseif ty=="int16" then
   out=mod(in,2^16);if out>2^15-1 then out=out-2^16;end
  elseif ty=="uint16" then
    out=mod(in,2^16);
  elseif ty=="int32" then
   out=mod(in,2^32);if out>2^31-1 then out=out-2^32;end
  elseif ty=="uint32" then
    out=mod(in,2^32);
  else
    error(ty+': data type not supported')
  end
endfunction

