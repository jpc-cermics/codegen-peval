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


function version=code_library()
  version="1.0"
endfunction

function code_insert(tag,varargin)
// insert a code instruction in the code list.
  global(code=list());
  if ~code_istag(tag) then 
    error(sprintf("Error: ""%s"" is not a code tag",tag));
  end
  code.add_last[list(tag,varargin(:))];
endfunction

function y=code_istag(tag)
// lists the possible tags which can be found in code 
  tags=['if_expr';'switch_expr';'annotation';'mcopy';'nop';
	'set';'assign';'bi_insert';'uni_insert';'endfunction';
        'callf';'ident'];
  y=or(tag==tags);
endfunction

function ok=code_check(code) 
// checks if a code expression is syntaxically correct.
  function r=are_expr(L,inds) 
    // cheks that L.sublist[inds] is a list of expressions 
    r=%t;
    for i=1:size(inds,'*')
      elt = L(inds(i));
      // hash(type=op,exp=list(operateur,args,optionel);
      // where args is a list of expressions or expression list 
      if ~(type(elt,'short')== 'h' && elt.type == "op") then 
	ok=%f;return;
      end
      if type(elt.exp,'short')<> 'l' then 
	ok=%f;return;
      end
      // This is unfinished
      // les arguments sont des expressions ou des variables
      if %f && not_vars(elt.exp(2),1:length(elt.exp(2)))  then 
	ok=%f;return;
      end
    end
  endfunction
  
  function r=are_vars(L,inds)
  // checks that L.sublist[inds] is a list of 'bvar'
    r=%t;
    for i=1:size(inds,'*')
      if type(L(inds(i)),'short')<> 'bvar' then 
	r=%f;return;
      end
    end
  endfunction
  
  // check the grammar of code 
  
  ok=%f;
  for i=1:length(code)
    // elt must be a list and first element is a string 
    elt = code(i);
    if type(elt,'short')== 'l' then 
      select elt(1) 
       case "nop" ; then 
	// list("nop");
       case "annotation"; then 
	// list("annotation",str)
	if ~(length(elt)== 2 && type(elt(2),'short')== 's') then 
	  error(sprintf("Element %d is wrong",i));
	  return;
	end
       case "set" then 
	// list("set",var,rhs)
	// rhs is an expression i.e see expr_check.
	if ~(length(elt)==3 && are_vars(elt,2) && are_expr(elt,3)) then 
	  error(sprintf("Element %d is wrong",i));
	  return;
	end
       case "bi_insert" then
	// list("bi_insert",var,i,j,rhs)
	if ~(length(elt)==5 && are_vars(elt,[2:4])) then 
	  error(sprintf("Element %d is wrong",i));
	  return;
	end
	if ~( are_vars(elt,5) || are_expr(elt,5)) then 
	  error(sprintf("Element %d is wrong",i));
	  return;
	end
       case "uni_insert" then
	// list("uni_insert",var,i,rhs);
	// rhs can be var ou exp 
	if ~(length(elt)==4 && are_vars(elt,[2:3])) then 
	  error(sprintf("Element %d is wrong",i));
	  return;
	end
	if ~( are_vars(elt,4) || are_expr(elt,4)) then 
	  error(sprintf("Element %d is wrong",i));
	  return;
	end
       case "assign" then 
	// list("assign",var,rhs)
	if ~(length(elt)==3 && are_vars(elt,[2:3])) then 
	  error(sprintf("Element %d is wrong",i));
	  return;
	end
       case "mcopy" then 
	// list("mcopy", var, in);
	if ~(length(elt)==3 && are_vars(elt,[2:3])) then 
	  error(sprintf("Element %d is wrong",i));
	  return;
	end
       case "if_expr" then
	// list("if_expr",cond,exp1 [,exp2])
	if ~((length(elt)==3 && are_vars(elt,[2:3])) || 
	  (length(elt)==4 && are_vars(elt,[2:4]))) then 
	  error(sprintf("Element %d is wrong",i));
	  return;
	end
       case "endfunction" then 
	// list("endfunction")
	if length(elt)<>1 then 
	  error(sprintf("Element %d is wrong",i));
	  return;
	end
       case "switch_expr" then 
	// list("switch_expr",cond,elt1,...,eltn)
	// where all the elti are ops of type call
       case "callf" then 
	// list("callf",expression("call",list("op",out-list,in-list)));
	// where expression is a call 
	if ~( length(elt)==2 && are_expr(elt,2)) then 
	  error(sprintf("Element %d is wrong",i));
	  return;
	end
       case "ident" then
      else
	error(sprintf("Error: %s is not a proper code tag",elt(1)));
	return
      end
    end
  end
  ok=%t;
endfunction

function count=code_countvar(code,inds,varname,count)
// Increases count whith the number of occurences of varname 
// found in code.sublist[inds].
  
  function count=countvarname(rhs,varname,count)
  // increases the value of count if varname is the name of 
  // rhs := 'bvar' | expression | list of rhs 
    if type(rhs,'short')=="bvar" && rhs.get_varname[]==varname then 
      count=count+1;
    elseif type(rhs,'short') =="h" && rhs.type == "op" then
      // loop on args 
      count=countvarname(rhs.exp(2),varname,count);
    elseif type(rhs,'short') == 'l' then 
      // loop on elements. could possibly use a map.
      for t=rhs; count=countvarname(t,varname,count);  end
    end
  endfunction

  function count=countvarname_test(rhs,varname,count)
    count1 = countvarname(rhs,varname,count);
    count2 = bvar_code_countvarname(rhs,varname,count);
    if count2<>count1 then pause xxx;end
    count=count2;
  endfunction
  
  for k=inds
    elt=code(k);
    if is_op(elt,'call') then  count =2;continue;  end
    select elt(1)
     case "nop" ; then 
     case "annotation"; then 
     case "set" then
      var=elt(2);rhs=elt(3);
      if var.get_varname[]==varname then count=2,break;end
      count=bvar_code_countvarname(rhs,varname,count);       
     case "bi_insert" then
      var=elt(2);
      if var.get_varname[]==varname then count=2,break,end
      count=bvar_code_countvarname(elt.sublist[3:5],varname,count);
     case "uni_insert" then
      var=elt(2);i=elt(3);rhs=elt(4)
      if var.get_varname[]==varname then count=2,break,end
      count=bvar_code_countvarname(elt.sublist[3:4],varname,count);
     case "assign" then
      var=elt(2);rhs=elt(3)
      if var.get_varname[]==varname then count=2,break,end
      count=bvar_code_countvarname(rhs,varname,count);
     case "mcopy" then 
      var=elt(2);in=elt(3)
      if var.get_varname[]==varname then count=2,break,end
      if in.get_varname[]==varname then count=2,break,end
     case "if_expr" then
      condi=elt(2);in1=elt(3);in2=elt(4)
      count=bvar_code_countvarname(elt.sublist[2:4],varname,count);
     case "endfunction" then 
     case "switch_expr" then 
     case "callf" then 
      // pas clair: ne faut-il pas des 2 ici XXX 
      // list("callf",expression("call",list("op",out-list,in-list)));
      count=bvar_code_countvarname(elt(2),varname,count);
    end
  end
endfunction

function code=code_replacevar_soft(code,inds,varname,newval)
// performs replacevarname in given code for indices 
// given by inds.
// Unused: replaced by hard coded function bvar_code_replacevar
// replacevarname can also be replaced by bvar_code_replacevarname 
  
  function rhs=replacevarname(rhs,varname,expr)
  // replaces rhs by expr if rhs has name varname
    if type(rhs,'short')=="bvar" && rhs.get_varname[]==varname then 
      rhs=(expr);
    elseif type(rhs,'short') =="h" && rhs.type == "op" then
      rhs.exp(2)=replacevarname(rhs.exp(2),varname,expr);
    elseif type(rhs,'short') == 'l' then 
      for i=1:length(rhs);rhs(i)= replacevarname(rhs(i),varname,expr);end;
    end
  endfunction

  if type(newval,'short')== "h" && newval.type == "op" && newval.exp(1)=="value" then
    newval= newval.exp(2)(1);
  end
  
  // function code 
  
  for ji=inds(:)'
    elt=code(ji)
    select elt(1)
     case "nop" ; then 
     case "annotation"; then 
     case  "set" then
      var=elt(2);rhs=elt(3)
      var=replacevarname(var,varname,newval);elt(2)=var
      rhs=replacevarname(rhs,varname,newval);elt(3)=rhs
     case "bi_insert" then
      var=elt(2);i=elt(3);j=elt(4);rhs=elt(5)
      var=replacevarname(var,varname,newval);elt(2)=var
      i=replacevarname(i,varname,newval);elt(3)=i
      j=replacevarname(j,varname,newval);elt(4)=j
      rhs=replacevarname(rhs,varname,newval);elt(5)=rhs
     case "uni_insert" then
      var=elt(2);i=elt(3);rhs=elt(4)
      var=replacevarname(var,varname,newval);elt(2)=var
      i=replacevarname(i,varname,newval);elt(3)=i
      rhs=replacevarname(rhs,varname,newval);elt(4)=rhs
     case "assign" then
      var=elt(2);rhs=elt(3)
      var=replacevarname(var,varname,newval);elt(2)=var
      rhs=replacevarname(rhs,varname,newval);elt(3)=rhs
     case "mcopy" then
      var=elt(2);in=elt(3)
      var=replacevarname(var,varname,newval);elt(2)=var
      in=replacevarname(in,varname,newval);elt(3)=in
     case "if_expr" then
      condi=elt(2);in1=elt(3);in2=elt(4)
      condi=replacevarname(condi,varname,newval);elt(2)=condi       
      in1=replacevarname(in1,varname,newval);elt(3)=in1    
      in2=replacevarname(in2,varname,newval);elt(4)=in2
     case "endfunction" then 
     case "switch_expr" then 
     case "callf" then 
      // list("callf",expression("call",list("op",out-list,in-list)));
      exp=replacevarname(elt(2),varname,newval);elt(2)=exp;
    end
    code(ji)=elt;
  end;
endfunction

function names=code_vars_soft(code)
// return a hash table of used and defined variables in code 
  
  function H=getvarname(expr);
    if type(expr,'short')=="bvar"  then 
      H=hash(1); H(expr.get_varname[])=%t;
    elseif type(expr,'short') =="h" && expr.type == "op" then
      // call getvarname on list of operands
      H=getvarname(expr.exp(2));
    elseif type(expr,'short') =='l' 
      // union of the results of getvarname for each element
      names=map(expr,getvarname);
      H=hash(length(names));
      for name = names; H.merge[name];end
    else
      H=hash(0);
    end
  endfunction
  
  names=hash(10);
  for ji=1:length(code)
    elt=code(ji)
    select elt(1)
     case "nop" ; then 
     case "annotation"; then 
     case "set" then
      names.merge[getvarname(elt.sublist[2:3])];
     case "bi_insert" then
      names.merge[getvarname(elt.sublist[2:5])];
     case "uni_insert" then
      names.merge[getvarname(elt.sublist[2:4])];
     case "assign" then
      names.merge[getvarname(elt.sublist[2:3])];
     case "mcopy" then
      names.merge[getvarname(elt.sublist[2:3])];
     case "if_expr" then
      names.merge[getvarname(elt.sublist[2:4])];
     case "endfunction" then 
     case "switch_expr" then 
     case "callf" then 
      names.merge[getvarname(elt.sublist[2])];
    end
  end
endfunction

function names=code_vars_used_soft(code)
// return a vector of used variables in given code 
  
  function names=getvarname(expr);
    if type(expr,'short')=="bvar"  then 
      names=expr.get_varname[]
    elseif type(expr,'short') =="h" && expr.type == "op" then
      // call getvarname on list of operands
      names=getvarname(expr.exp(2));
    elseif type(expr,'short') =='l' 
      // union of the results of getvarname for each element
      names_t=map(expr,getvarname);
      names=[]
      for name = names_t; names=[names,name];end
    else
      names=[]
    end
  endfunction
  
  names=[];
  for ji=1:length(code)
    elt=code(ji)
    select elt(1)
     case "nop" ; then 
     case "annotation"; then 
     case "set" then
      names=[names,getvarname(elt.sublist[3])];
     case "bi_insert" then
      names=[names,getvarname(elt.sublist[3:5])];
     case "uni_insert" then
      names=[names,getvarname(elt.sublist[3:4])];
     case "assign" then
      names=[names,getvarname(elt.sublist[3])];
     case "mcopy" then
      names=[names,getvarname(elt.sublist[3])];
     case "if_expr" then
      names=[names,getvarname(elt.sublist[3:4])];
     case "endfunction" then 
     case "switch_expr" then 
     case "callf" then 
      // names=[names,getvarname(elt.sublist[2])];
    end
  end
endfunction

function count=code_countexp(code,inds,exp)
  
  function count=countexp(rhs,to_subst,count)
  // rhs is supposed to be mlist(['op',...)
  // counts the number of b_extract matching to_subst in rhs
    if  type(rhs,'short') =="h" && rhs.type == "op" then
      op=rhs.exp(1) // operator name 
      operands=rhs.exp(2) // arguments 
      if op=="b_extract" && ...
	    to_subst(1).equal[operands(1).get_varname[]] && ...
	    to_subst(2).equal[operands(2)] && ...
	    to_subst(3).equal[operands(3)] then 
	count=count+1; return;
      else
	for t=operands; count=countexp(t,to_subst,count); end
      end
    end
  endfunction
  count=0;
  for ji=inds
    elt=code(ji)
    select elt(1)
     case "nop" ; then 
     case "annotation"; then 
     case "set" then
      var=elt(2);rhs=elt(3)
      count=countexp(rhs,to_subst,count);
     case "bi_insert" then
      var=elt(2);i=elt(3);j=elt(4);rhs=elt(5);
      if var.get_varname[]==to_subst(1) && i.equal[to_subst(2)] && j.equal[to_subst(2)] then
	count=2,break,
      end
      count=countexp(i,to_subst,count);
      count=countexp(j,to_subst,count);
      count=countexp(rhs,to_subst,count);
     case "uni_insert" then
      var=elt(2);i=elt(3);rhs=elt(4)
      count=countexp(i,to_subst,count);
      count=countexp(rhs,to_subst,count);
     case "assign" then
      var=elt(2);rhs=elt(3);
      count=countexp(rhs,to_subst,count);
     case "mcopy" then
     case "if_expr" then
     case "endfunction" then 
     case "switch_expr" then 
     case "callf" then 
      count=countexp(elt(2),to_subst,count);
    end
  end
endfunction

function [Idx_used,Idx_modified] = code_varstatus(code,varname)
//
  function ok=isvarname(rhs,varname)
  // checks if varname is in rhs 
    if type(rhs,'short')=="bvar" then 
      ok = (rhs.get_varname[]==varname); 
    elseif  type(rhs,'short') =="h" && rhs.type == "op" then
      ok=isvarname(rhs.exp(2),varname);
    elseif type(rhs,'short')=='l' then 
      ok = %f;
      for e=rhs; 
        ok=isvarname(e,varname);
        if ok then return;end;
      end
    else
      ok = %f
    end
  endfunction
  
  function ok=isvarname_new(rhs,varname)
    ok1 = isvarname(rhs,varname);
    ok2 = bvar_code_isvarname(rhs,varname);
    if ~ok1.equal[ok2] then pause;end 
    ok = ok2
  endfunction
    
  Idx_used=zeros(1,0);  Idx_modified=zeros(1,0);
  for ji=1:length(code)
    ok_used=%f; ok_modified=%f
    elt=code(ji);
    select elt(1)
     case "nop" ; then 
     case "annotation"; then 
     case "set" then
      var=elt(2);rhs=elt(3)
      ok_used=bvar_code_isvarname(rhs,varname)
      ok_modified=bvar_code_isvarname(var,varname)
     case "bi_insert" then
      var=elt(2)
      ok_used=bvar_code_isvarname(elt.sublist[3:5],varname)
      ok_modified=bvar_code_isvarname(var,varname)
     case "uni_insert" then
      var=elt(2)
      ok_used=bvar_code_isvarname(elt.sublist[3:4],varname)
      ok_modified=bvar_code_isvarname(var,varname)
     case "assign" then
      var=elt(2);rhs=elt(3)
      ok_used=bvar_code_isvarname(rhs,varname)
      ok_modified=bvar_code_isvarname(var,varname)
     case "mcopy" then
      var=elt(2);in=elt(3)
      ok_used=bvar_code_isvarname(in,varname)
      ok_modified=bvar_code_isvarname(var,varname)
     case "if_expr" then
      condi=elt(2);in1=elt(3);in2=elt(4)
      ok_used=bvar_code_isvarname(elt.sublist[2:4],varname);
      ok_modified=bvar_code_isvarname(elt.sublist[3:4],varname)        
     case "endfunction" then 
     case "switch_expr" then 
     case "callf" then 
      // list("callf",expression("call",list("op",out-list,in-list)));
      ins=elt(2).exp(2)(3); // in-list
      ok_used=bvar_code_isvarname(ins,varname);
      outs=elt(2).exp(2)(2);// out-list
      ok_modified=bvar_code_isvarname(outs,varname);
    end
    if ok_used then Idx_used=[Idx_used,ji],end;
    if ok_modified then Idx_modified=[Idx_modified,ji],end
  end
endfunction

function [ncode] = code_removenop(code)
//
  n=length(code);
  Idx=[];
  for ic=1:length(code)
    if code(ic)(1) <> "nop" then 
      Idx.concatd[ic];
    end
  end
  ncode=code.sublist[Idx];
endfunction
