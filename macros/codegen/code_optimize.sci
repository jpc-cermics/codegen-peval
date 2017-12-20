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
// translated to nsp : Jean-Philippe Chancelier (ENPC).
//
// Authors: J.-Ph. Chancelier (Cermics/ENPC), R. Nikoukhah (Altair Engineering)



function [Lres,code,declarations]=code_optimize(Lres,code,declarations,top_declarations,opt=[%f,%f,%f],nopt=6,verbose=%f,check=%f)
// 
  
  function ok=contains_mutable(rhs,top_declarations,declarations,ok)
  // unused 
    if nargin < 4 then ok=%f;end
    if type(rhs,'short')=="bvar" && (is_in_top(rhs.get_varname[],top_declarations) ...
				     |is_io(rhs.get_varname[],declarations)) then
      ok=%t;return
    elseif type(rhs,'short') =="h" && rhs.type == "op" then
      operands=rhs.exp(2)
      for t=operands
	ok=contains_mutable(t,top_declarations,declarations,ok)
	if ok then return;end
      end
    end
  endfunction

  function declarations=replace_io(varname,name,declarations)
  // unused
    for i=1:length(declarations)
      t=declarations(i)
      if t(1)=="function" then
	io=t(3);
	if io.iskey[varname] then 
	  v = io(varname);v.set_varname[name];
	  io.delete[varname];
	  io(name)=v;
	  id=find(io.values==varname);io.values[id]=name;
	  t(3)=io;
	  declarations(i)=t
	end
      end
    end
  endfunction

  function y=is_io(name,declarations)
  // 
    y=%f
    for i=1:length(declarations)
      t=declarations(i)
      if t(1)=="function" then
	io=t(3);
	if io.iskey[name] then y=%t;return;end 
      end
    end
  endfunction

  function ok=is_in_top(varname,top_declarations)
    ok=%f
    for ik=1:length(top_declarations)
      td=top_declarations(ik);
      if varname==td(2).get_varname[] then ok=%t;break,end
    end
  endfunction
  
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
  
  function [code,declarations,Lovar]=simplify_code1(code,declarations,top_declarations,Livar,verbose=%f)
    
    function [vars,ins,Idx]=DefinedVars(code)
      Idx=[];vars=list();ins=list()
      for ji=1:length(code)
	ttt=code(ji)
	if or(ttt(1)==["set","mcopy"]) then  // ONLY TOPLEVEL, whatabout if and switch
	  var=ttt(2);in=ttt(3)
	  vars($+1)=var; 
	  ins($+1)=in
	  Idx=[Idx,ji]
	end
      end
    endfunction
    
    if nargin >= 4 then 
      Lovar = Livar;
    else Lovar = list(numerics(0));
    end
     
    done=%f;
    while ~done
      done=%t
      [vars,ins,Idx]=DefinedVars(code)  
      // we have instructions like vars(i)=ins(i) at code line Idx(i) 
      // we check if vars(i) can be replaced by ins(i)
      for i=1:length(vars)
	var=vars(i)
	in=ins(i);
	if type(in,'short')=="bvar" && type(var,'short')=="bvar"   then
	  iname=in.get_varname[];
	  varname=var.get_varname[];
	  id=Idx(i)
	  [vIdx_used,vIdx_modified] = code_varstatus(code,varname);
	  Idv = union(vIdx_used,vIdx_modified);
	  [iIdx_used,iIdx_modified] = code_varstatus(code,iname);
	  Idi = union(iIdx_used,iIdx_modified);
	  // tag is true if 
	  //    v is modified or used after  id (~or(Idv<id))
	  //    i is modified or used before id (~or(Idi>id))
	  // Thus we can replace v by i in Idv 
	  // The fact that v is or is not in top and io should not change ? 
	  tag = ~or(Idv<id) && ~or(Idi>id); 
	  if tag && (~is_in_top(varname,top_declarations) &&  ~is_io(varname,declarations)) then 
	    // we can replave v by i in Idv 
	    if verbose then printf("%s\n","Replace "+varname+" with "+iname);end
	    code(id)=list("nop");
	    // take care to propagate name replacement to output
            // variables names 
	    if nargin >= 4 then 
	      for iv=1:length(Livar) 
		if Livar(iv).get_varname[]== varname then Lovar(iv).set_varname[iname]; end
	      end
	    end
	    code=bvar_code_replacevar(code,Idv,varname,in);
	    done=%f;
	    break
	  end
	end
      end
    end
    // Hash table of used variables 
    usedvars=bvar_code_vars(code);
    // insert ivar       
    if nargin >= 4 then 
      for iv=1:length(Livar); 
	if type(Lovar(iv),'short') == 'bvar' then 	  
	  usedvars(Lovar(iv).get_varname[])=%t; 
	end
      end
    end
    declarations=RemoveUnused(declarations,usedvars);
    code=code_removenop(code);
  endfunction
 
  function [code,declarations,Lovar]=simplify_code2(code,declarations,top_declarations,Livar,verbose=%f)
  // 
    if nargin >= 4 then Lovar = Livar;
    else Lovar = list(numerics(0));end
      
      for ii=1:length(code)
	tt=code(ii)
	if or(tt(1)==["set","assign"]) then
	  if ~is_op(tt(3),"If_exp") then
	    var=tt(2);xrhs=tt(3)
	    varname=var.get_varname[]
	    // take care not to remove an element of Lovar 
	    count=0;
	    for iv=1:length(Lovar) 
	      count=max(count,2*(varname.equal[Lovar(iv).get_varname[]]));
	    end
	    count=code_countvar(code,ii+1:length(code),varname,count);
	    if ~is_io(varname,declarations) && ~is_in_top(varname,top_declarations) ...
		  && count==0 & tt(1)=="set" then 
	      if verbose then printf("%s\n","remove "+varname);end
	      del=[];
	      for jk=1:length(declarations)
		td=declarations(jk)
		if td(1)<>"function" && td(1)<>"nop" && td(1) <> "annotation" ...
		      && td(2).get_varname[]==varname then del=[del,jk],end
	      end
	      for deli=del($:-1:1),  declarations(deli)=null(),end
	      code(ii)=list("nop")           

	    elseif  count==1 && type(xrhs,'short')=="bvar" && ~is_in_top(xrhs.get_varname[],top_declarations) && ~is_io(xrhs.get_varname[],declarations) then
	      if verbose then printf("%s\n","Do replace "+xrhs.get_varname[]+" with "+varname);end
	      varname=xrhs.get_varname[];xrhs=tt(2)
 	      code(ii)=list("nop");
	      code=bvar_code_replacevar(code,1:length(code),varname,xrhs);
	    elseif count==1 && ~is_io(varname,declarations) &&~is_in_top(varname,top_declarations)  then
              // usedvars=code_vars_used_soft(list(tt)); 
	      usedvars=bvar_code_vars_used(list(tt)); 
	      usedvars=usedvars.__keys';
	      rep=%t
              for vari=usedvars
                [variIdx_used,variIdx_modified] = code_varstatus(list(code(ii+1:$)),vari);
                if ~isempty(variIdx_modified) then rep=%f;break;end
              end
              if rep then
	        if verbose then printf("%s\n","remove "+varname+" by substitution");end
                precode=code
	        code(ii)=list("nop")
	        [code,st]=bvar_code_replacevar(code,ii+1:length(code),varname,xrhs);
                if st then 
                  code=precode;
                  if verbose then printf("%s\n","Failed substitution because inside callf.");end
                end   
              end
	    end
	  end
	end
	usedvars=bvar_code_vars(code);
	// insert ivar       
	if nargin >= 4 then 
	  for iv=1:length(Livar); usedvars(Livar(iv).get_varname[])=%t; end
	end
	declarations=RemoveUnused(declarations,usedvars);
      end
  endfunction

  function [code,declarations,Lovar]=simplify_code3(code,declarations, top_declarations,Livar,verbose=%f)
    
    function rhs=replaceexp(rhs,to_subst,expr)
    // replace the b_extract by expr if they match to_subst
    // 
      if  type(rhs,'short') =="h" && rhs.type == "op" then
	op=rhs.exp(1)
	operands=rhs.exp(2)
	if op=="b_extract" && ...
	      to_subst(1).equal[operands(1).get_varname[]] && ...
	      to_subst(2).equal[operands(2)] && ...
	      to_subst(3).equal[operands(3)] then 
	  rhs = expr; return;
	else
	  newoperands=list()
	  for t=operands
	    tn=replaceexp(t,to_subst,expr)
	    newoperands($+1)=tn
	  end
	  rhs.exp(2)=newoperands
	end
      end
    endfunction
    
    if nargin >= 4 then 
      Lovar = Livar;
    else 
      Lovar = list(numerics(0));
    end
    //      for ii=1:length(code)
    for ii=length(code):-1:1
      tt=code(ii)
      if or(tt(1)==["bi_insert"]) then
	varname=tt(2).get_varname[];
	if and(type(tt(3),'short')<>["bvar","op"]) && and(type(tt(4),'short')<>["bvar","op"]) then
	  to_subst=list(varname,tt(3),tt(4))
	  xrhs=tt(5)
	  if ~is_io(varname,declarations) && ~is_in_top(varname,top_declarations) then
	    [Idx_used,Idx_modified] = code_varstatus(list(code(ii+1:$)),varname)
	    if isempty(Idx_modified) then
	      count=code_countexp(code,ii+1:length(code),to_subst);

	      if count==1 || or(type(xrhs,"short")==["bvar","m","i","b"]) then 
		if verbose then printf("%s\n","replace "+to_subst(1)+"("+ ...
				       string(to_subst(2))+","+string(to_subst(3))+") by substitution");end 

		  code(ii)=list("nop")
		  for ji=ii+1:length(code)
		    ttt=code(ji)
		    if ttt(1)=="set" then
		      var=ttt(2);rhs=ttt(3)
		      rhs=replaceexp(rhs,to_subst,xrhs);ttt(3)=rhs
		    elseif ttt(1)=="bi_insert" then
		      var=ttt(2);i=ttt(3);j=ttt(4);rhs=ttt(5)
		      i=replaceexp(i,to_subst,xrhs);ttt(3)=i
		      j=replaceexp(j,to_subst,xrhs);ttt(4)=j
		      rhs=replaceexp(rhs,to_subst,xrhs);ttt(5)=rhs
		    elseif ttt(1)=="uni_insert" then
		      var=ttt(2);i=ttt(3);rhs=ttt(4)
		      i=replaceexp(i,to_subst,xrhs);ttt(3)=i
		      rhs=replaceexp(rhs,to_subst,xrhs);ttt(4)=rhs
		    elseif ttt(1)=="assign" then
		      var=ttt(2);rhs=ttt(3)
		      rhs=replaceexp(rhs,to_subst,xrhs);ttt(3)=rhs
		    end
		    code(ji)=ttt
		  end
		end
	      end
	    end
	  end
	end
      end
      // clear unused declared variables
      del=[];
      for kk=1:length(declarations)
	count=0;
	td=declarations(kk)
	if td(1)<>"function" && td(1)<>"nop" && td(1) <> "annotation" then 
	  if type(td(2),'short')=='bvar' then
	    varname=td(2).get_varname[]
	    count=code_countvar(code,1:length(code),varname,count);
	    for iv=1:length(Lovar) 
	      if varname.equal[Lovar(iv).get_varname[]] then count=2;break;end 
	    end
	    if is_io(varname,declarations) then count=0,end 
	  end
	else
	  count=2
	end
	if count==0 then del=[del,kk],end
      end
      for d=del($:-1:1), declarations(d)=null();end
    endfunction

  // 
  if check then  ok= code_check(code);end
  
  for ki=1:nopt 
    if or(opt) then 
      if verbose then printf("optimization iteration: %d\n",ki);end
      if opt(1) then 
	T=timer();
	[code,declarations,Lres]=simplify_code1(code,declarations,top_declarations,Lres,verbose=verbose);
	if check then ok= code_check(code);end 
	if verbose then printf("optimization step1: cpu=%7.5f\n",timer());end 
      end
      if opt(2) then 
	T=timer();
	[code,declarations,Lres]=simplify_code2(code,declarations,top_declarations,Lres,verbose=verbose);
	if check then ok= code_check(code);end 
	if verbose then printf("optimization step2: cpu=%7.5f\n",timer());end 
      end
      if opt(3) then 
	T=timer();
	[code,declarations,Lres]=simplify_code3(code,declarations,top_declarations,Lres,verbose=verbose);
	if check then ok= code_check(code);end 
	if verbose then printf("optimization step3: cpu=%7.5f\n",timer());end
      end
    end
  end
endfunction

if %f then 
  // unused 
  
  function [code,declarations]=fix_outs_unused(code,declarations)
  // a revoir XXXX 
    function expr=substitutevarname(expr,oldvarname,varname)
    // changes the name of variables names oldvarname to varname 
    // in expr.
      pause in substitutevarname
      if type(expr,'short')=="bvar" && expr.get_varname[]==oldvarname then 
	expr.set_varname[varname];
      elseif  type(expr,'short') =="h" && expr.type == "op"  then
	newoperands=list()
	for t=expr.exp(2)
	  newoperands($+1)=substitutevarname(t,oldvarname,varname);
	end
	expr.exp(2)=newoperands
      end
    endfunction
    
    for kk=1:length(declarations)
      t=declarations(kk);
      if t(1)=="function" then
	fname=t(2);io=t(3);
	// io is a hash table 
	args = io.values;// __keys;
	new_io = hash(size(io,'*'));
	new_io.values=m2s([]);
	for i1=1:size(args,'*')
	  vari= io(args(i1));
	  // vari is a bvar 
	  if size(vari.get_value[],'*') ==1 then
	    varname=vari.get_varname[];
	    newname="*"+varname
	    vari.set_varname[newname];
	    new_io(newname) = vari;
	    new_io.values($+1,1)=newname;
	    for ji=1:length(code)
	      ttt=code(ji)
	      if ttt(1)=="set" then
		var=ttt(2);rhs=ttt(3);
		var=substitutevarname(var,varname,newname);ttt(2)=var;
		rhs=substitutevarname(rhs,varname,newname);ttt(3)=rhs;
	      elseif ttt(1)=="bi_insert" then
		var=ttt(2);i=ttt(3);j=ttt(4);rhs=ttt(5);
		var=substitutevarname(var,varname,newname);ttt(2)=var;
		i=substitutevarname(i,varname,newname);ttt(3)=i
		j=substitutevarname(j,varname,newname);ttt(4)=j
		rhs=substitutevarname(rhs,varname,newname);ttt(5)=rhs;
	      elseif ttt(1)=="uni_insert" then
		var=ttt(2);i=ttt(3);rhs=ttt(4);
		var=substitutevarname(var,varname,newname);ttt(2)=var;
		i=substitutevarname(i,varname,newname);ttt(3)=i
		rhs=substitutevarname(rhs,varname,newname);ttt(4)=rhs;
	      elseif ttt(1)=="assign" then
		var=ttt(2);rhs=ttt(3)
		var=substitutevarname(var,varname,newname);ttt(2)=var;
		rhs=substitutevarname(rhs,varname,newname);ttt(3)=rhs;
	      elseif ttt(1)=="mcopy" then
		var=ttt(2);in=ttt(3);
		in=substitutevarname(in,varname,newname);ttt(3)=in;
	      elseif is_op(ttt,'call') then 
		t=ttt.exp;
		// list("call,list(fname,outs,ins))
		outs=t(2)(2)
		for ik=1:length(outs)
		  outs(ik)=substitutevarname(outs(ik),varname,newname);
		end
		t(2)(2)=outs
		ins=t(2)(3)
		for ik=1:length(ins)
		  ins(ik)=substitutevarname(ins(ik),varname,newname);
		end
		t(2)(3)=ins
		ttt.exp=t;
	      end
	      code(ji)=ttt;
	    end
	  else 
	    varname=vari.get_varname[];
	    new_io(varname) = vari;
	    new_io.values($+1,1)=varname;
	  end
	end
	t(3)=new_io; 
	declarations(kk)=t;
      end
    end
  endfunction
end






