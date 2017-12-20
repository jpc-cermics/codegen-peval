// Copyright 2013-2014 Enpc
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
// Authors: J.-Ph. Chancelier (Cermics/ENPC)

load_toolbox('codegen-peval');
load_toolbox('scicos-4.4');

// select which optimization step to perform 
opt=[%f,%f,%f];
opt=[%t,%t,%f];
nopt=6; // number of calls to optimization 
ntest=10;
if ~exists('logfile') then logfile=[] end;
  
// nsp 
[ok,fn,txt]=code_test(f,code_test_data(),opt=opt,target="nsp",verbose=%f,logfile=logfile);
if ~ok then pause;end
for i=1:ntest 
  y=code_test_data();
  res1=fn(y(:));res2=f(y(:));
  if type(res2,'short') == 'bvar' then res2=res2.get_value[];end 
  if abs(res1-res2) > 1000*%eps then pause;end
end

// C 
[ok,fn,txt]=code_test(f,code_test_data(),opt=opt,target="C",verbose=%f,logfile=logfile);
if ~ok then pause;end
for i=1:ntest 
  y=code_test_data();
  res1=fn(y(:));res2=f(y(:));
  if type(res2,'short')== 'bvar' then res2=res2.get_value[];end 
  if abs(res1-res2) > 1000*%eps then pause;end
end

// P 
[ok,fn,txt]=code_test(f,code_test_data(),opt=opt,target="P",verbose=%f,logfile=logfile);
if ~ok then pause;end
for i=1:ntest 
  y=code_test_data();
  res1=fn(y(:));res2=f(y(:));
  if type(res2,'short')== 'bvar' then res2=res2.get_value[];end 
  if abs(res1-res2) > 1000*%eps then pause;end
end

