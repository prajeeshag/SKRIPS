clear
%Lon=32.0071+0.02*10:0.02:32.0071+0.02*2249;
Lon=32.0071:0.02:32.0071+0.02*2249;
Lat=-2.6129+0.02*10:0.02:-1.6129+0.02*1599;
[XX,YY]=meshgrid(Lon(1:1:end),Lat(1:1:end));
XX=XX';
YY=YY';

vertical_coor = ...
       [1.,1.,2.,2.,3.,3.,4.,4.,5.,5.,...    
        5.,6.,6.,6.,7.,7.,8.,8.,9.,9.,...   
        10.,11.,12.,13.,14.,15.,16.,18.,20.,21.,...
        23.,25.,28.,30.,32.,35.,38.,42.,45.,49.,...
        53.,58.,63.,68.,74.,81.,88.,95.,103.,112.,...
        122.,140.,160.,185.,210.,240.,270.,300.,340.,380.,...
        450.,500.,600.,750.];
for i = 1 : 64
    boundary_depth ( i ) = sum ( vertical_coor ( 1 : i ) );       
end
body_depth = boundary_depth - vertical_coor / 2;  


%%




testT=squeeze(ncread('ArabianSea_daily_19930101.nc','thetao'));
testS=squeeze(ncread('ArabianSea_daily_19930101.nc','so'));
testU=squeeze(ncread('ArabianSea_daily_19930101.nc','uo'));
testV=squeeze(ncread('ArabianSea_daily_19930101.nc','vo'));
testE=squeeze(ncread('ArabianSea_daily_19930101.nc','zos'));
lon_8km=ncread('ArabianSea_daily_19930101.nc','longitude');
lat_8km=ncread('ArabianSea_daily_19930101.nc','latitude');
dep_8km=ncread('ArabianSea_daily_19930101.nc','depth');
[XI,YI]=meshgrid(Lon(1:1:end),Lat(1:1:end));
XI=XI';
YI=YI';
 
[xxx_8km,yyy_8km,zzz_8km]=meshgrid(lon_8km,lat_8km,dep_8km);
    
[mm nn oo]=size(testT);

for i=1:oo
    i
    if sum(sum(isnan(testT(:,:,i))))~=mm*nn
    %%%% T/S
    for j=1:nn
        position_ocean=isnan(testT(:,j,i))~=1;
        position_land=isnan(testT(:,j,i))==1;
        if sum(position_ocean)>1 
            temp=testT(:,j,i);
            temp(position_land)=interp1(XI(position_ocean,j),testT(position_ocean,j,i),XI(position_land,j),'nearst','extrap');
            testT(:,j,i)=temp;
            
            temp=testS(:,j,i);
            temp(position_land)=interp1(XI(position_ocean,j),testS(position_ocean,j,i),XI(position_land,j),'nearst','extrap');
            testS(:,j,i)=temp;
        elseif sum(position_ocean)<=1 && j>1
            testT(:,j,i)=testT(:,j-1,i);
            testS(:,j,i)=testS(:,j-1,i);
        end
    end
    else
        testT(:,:,i)=testT(:,:,i-1);
        testS(:,:,i)=testS(:,:,i-1);
    end
    %%%% U
    if sum(sum(isnan(testU(:,:,i))))~=mm*nn
    
    for j=1:nn
        position_ocean=isnan(testU(:,j,i))~=1;
        position_land=isnan(testU(:,j,i))==1;
        if sum(position_ocean)>1 
            temp=testU(:,j,i);
            temp(position_land)=interp1(XI(position_ocean,j),testU(position_ocean,j,i),XI(position_land,j),'nearst','extrap');
            testU(:,j,i)=temp;
        elseif sum(position_ocean)<=1 && j>1
            testU(:,j,i)=testU(:,j-1,i);
        end
    end    
    else
        testU(:,:,i)=testU(:,:,i-1);
    end
    %%%% V
    if sum(sum(isnan(testV(:,:,i))))~=mm*nn
    for j=1:nn
        position_ocean=isnan(testV(:,j,i))~=1;
        position_land=isnan(testV(:,j,i))==1;
        if sum(position_ocean)>1 
            temp=testV(:,j,i);
            temp(position_land)=interp1(XI(position_ocean,j),testV(position_ocean,j,i),XI(position_land,j),'nearst','extrap');
            testV(:,j,i)=temp;
        elseif sum(position_ocean)<=1 && j>1
            testV(:,j,i)=testV(:,j-1,i);
        end
    end
    else
        testV(:,:,i)=testV(:,:,i-1);
    end
end



testT=permute(testT,[2 1 3]);
testS=permute(testS,[2 1 3]);
testU=permute(testU,[2 1 3]);
testV=permute(testV,[2 1 3]);

[XX_2km,YY_2km,ZZ_2km] = meshgrid(Lon,Lat,body_depth);
iniT=interp3(xxx_8km,yyy_8km,zzz_8km,testT,XX_2km,YY_2km,ZZ_2km);
iniS=interp3(xxx_8km,yyy_8km,zzz_8km,testS,XX_2km,YY_2km,ZZ_2km);
iniU=interp3(xxx_8km,yyy_8km,zzz_8km,testU,XX_2km,YY_2km,ZZ_2km);
iniV=interp3(xxx_8km,yyy_8km,zzz_8km,testV,XX_2km,YY_2km,ZZ_2km);
% 
% 
% iniU(:,:,49)=iniU(:,:,48);
% iniU(:,:,50)=iniU(:,:,48);
% 
% iniV(:,:,49)=iniV(:,:,48);
% iniV(:,:,50)=iniV(:,:,48);


iniT=permute(iniT,[2 1 3]);
iniS=permute(iniS,[2 1 3]);
iniU=permute(iniU,[2 1 3]);
iniV=permute(iniV,[2 1 3]);


wrslice('./Ini_T_AS2km_19930101.bin',iniT,1,'real*4')
wrslice('./Ini_S_AS2km_19930101.bin',iniS,1,'real*4')
wrslice('./Ini_U_AS2km_19930101.bin',iniU,1,'real*4')
wrslice('./Ini_V_AS2km_19930101.bin',iniV,1,'real*4')

%%

for i=1:1
    i
    %%%% E
    for j=1:nn
        position_ocean=isnan(testE(:,j,i))~=1;
        position_land=isnan(testE(:,j,i))==1;
        if sum(position_ocean)>1 
            temp=testE(:,j,i);
            temp(position_land)=interp1(XI(position_ocean,j),testE(position_ocean,j,i),XI(position_land,j),'nearst','extrap');
            testE(:,j,i)=temp;


        elseif sum(position_ocean)<=1 && j>1
            testE(:,j,i)=testE(:,j-1,i);            
        end
    end

end

testE=permute(testE,[2 1]);
iniE=interp2(xxx_8km(:,:,1),yyy_8km(:,:,1),testE,XX_2km(:,:,1),YY_2km(:,:,1));
iniE=permute(iniE,[2 1]);
wrslice('./Ini_E_AS2km_19930101.bin',iniE,1,'real*4')

kankan=rdslice('Ini_U_AS2km_19930101.bin',[2240 1640 64],1,'real*4');