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


[xx_south_2km,zz_south_2km]=meshgrid(Lon,body_depth);
[xx_east_2km ,zz_east_2km ]=meshgrid(Lat,body_depth);

out_path='/data/ArabianSea_for_cda/';

lon_8km=double(ncread([out_path,'ArabianSea_daily_20150101.nc'],'longitude'));
lat_8km=double(ncread([out_path,'ArabianSea_daily_20150101.nc'],'latitude'));
dep_8km=double(ncread([out_path,'ArabianSea_daily_20150101.nc'],'depth'));

[xx_south_8km,zz_south_8km]=meshgrid(lon_8km,dep_8km);
[xx_east_8km ,zz_east_8km ]=meshgrid(lat_8km,dep_8km);
%%
daten=datevec(datenum(1993,1,1)+[0:9861-1]);
%daten=datevec(datenum(2015,1,1)+[0:8]);

% for ii=1:length(daten(:,1))
%     if exist([out_path,'ArabianSea_daily_',num2str(daten(ii,1)),num2str(daten(ii,2),'%2.2d'),num2str(daten(ii,3),'%2.2d'),'.nc'])
%         continue
%     else
%         ii
%     end
% end

outputT_south=zeros(length(body_depth),length(Lon),length(daten(:,1)));
outputS_south=zeros(length(body_depth),length(Lon),length(daten(:,1)));
outputU_south=zeros(length(body_depth),length(Lon),length(daten(:,1)));
outputV_south=zeros(length(body_depth),length(Lon),length(daten(:,1)));
outputT_east =zeros(length(body_depth),length(Lat),length(daten(:,1)));
outputS_east =zeros(length(body_depth),length(Lat),length(daten(:,1)));
outputU_east =zeros(length(body_depth),length(Lat),length(daten(:,1)));
outputV_east =zeros(length(body_depth),length(Lat),length(daten(:,1)));


n=0;

for i=1:length(daten(:,1))
    i

    testT=squeeze(ncread([out_path,'ArabianSea_daily_',num2str(daten(i,1)),num2str(daten(i,2),'%2.2d'),num2str(daten(i,3),'%2.2d'),'.nc'],'thetao'));
    testS=squeeze(ncread([out_path,'ArabianSea_daily_',num2str(daten(i,1)),num2str(daten(i,2),'%2.2d'),num2str(daten(i,3),'%2.2d'),'.nc'],'so'));
    testU=squeeze(ncread([out_path,'ArabianSea_daily_',num2str(daten(i,1)),num2str(daten(i,2),'%2.2d'),num2str(daten(i,3),'%2.2d'),'.nc'],'uo'));
    testV=squeeze(ncread([out_path,'ArabianSea_daily_',num2str(daten(i,1)),num2str(daten(i,2),'%2.2d'),num2str(daten(i,3),'%2.2d'),'.nc'],'vo'));
    
    southT=double(squeeze(testT(:,32,:)));
    southS=double(squeeze(testS(:,32,:)));
    southU=double(squeeze(testU(:,32,:)));
    southV=double(squeeze(testV(:,32,:)));

    eastT=double(squeeze(testT(541,:,:)));
    eastS=double(squeeze(testS(541,:,:)));
    eastU=double(squeeze(testU(541,:,:)));
    eastV=double(squeeze(testV(541,:,:)));    
    for j=1:50
        position_ocean=isnan(southT(:,j))~=1;
        if sum(position_ocean)>1
            obcsT_south(:,j)=interp1(xx_south_8km(j,position_ocean),southT(position_ocean,j),xx_south_8km(j,:),'nearst','extrap');
            obcsS_south(:,j)=interp1(xx_south_8km(j,position_ocean),southS(position_ocean,j),xx_south_8km(j,:),'nearst','extrap');
        else
            obcsT_south(:,j)=obcsT_south(:,j-1);
            obcsS_south(:,j)=obcsS_south(:,j-1);
        end
        
        position_ocean=isnan(southU(:,j))~=1;
        if sum(position_ocean)>1
        obcsU_south(:,j)=interp1(xx_south_8km(j,position_ocean),southU(position_ocean,j),xx_south_8km(j,:),'nearst','extrap');
        else
            obcsU_south(:,j)=obcsU_south(:,j-1);
        end
        
        position_ocean=isnan(southV(:,j))~=1;
        if sum(position_ocean)>1
        obcsV_south(:,j)=interp1(xx_south_8km(j,position_ocean),southV(position_ocean,j),xx_south_8km(j,:),'nearst','extrap');
        else
            obcsV_south(:,j)=obcsV_south(:,j-1);
        end
    end
    
    for j=1:50
        position_ocean=isnan(eastT(:,j))~=1;
        if sum(position_ocean)>1
            obcsT_east(:,j)=interp1(xx_east_8km(j,position_ocean),eastT(position_ocean,j),xx_east_8km(j,:),'nearst','extrap');
            obcsS_east(:,j)=interp1(xx_east_8km(j,position_ocean),eastS(position_ocean,j),xx_east_8km(j,:),'nearst','extrap');
        else
            obcsT_east(:,j)=obcsT_east(:,j-1);
            obcsS_east(:,j)=obcsS_east(:,j-1);
        end
        
        position_ocean=isnan(eastU(:,j))~=1;
        if sum(position_ocean)>1
        obcsU_east(:,j)=interp1(xx_east_8km(j,position_ocean),eastU(position_ocean,j),xx_east_8km(j,:),'nearst','extrap');
        else
            obcsU_east(:,j)=obcsU_east(:,j-1);
        end
        
        position_ocean=isnan(eastV(:,j))~=1;
        if sum(position_ocean)>1
        obcsV_east(:,j)=interp1(xx_east_8km(j,position_ocean),eastV(position_ocean,j),xx_east_8km(j,:),'nearst','extrap');
        else
            obcsV_east(:,j)=obcsV_east(:,j-1);
        end
    end
    
    n=n+1
    outputT_south(:,:,n)=griddata(xx_south_8km,zz_south_8km,double(obcsT_south)',xx_south_2km,zz_south_2km);
    outputS_south(:,:,n)=griddata(xx_south_8km,zz_south_8km,double(obcsS_south)',xx_south_2km,zz_south_2km);
    outputU_south(:,:,n)=griddata(xx_south_8km,zz_south_8km,double(obcsU_south)',xx_south_2km,zz_south_2km);
    outputV_south(:,:,n)=griddata(xx_south_8km,zz_south_8km,double(obcsV_south)',xx_south_2km,zz_south_2km);

    outputT_east(:,:,n)=griddata(xx_east_8km,zz_east_8km,double(obcsT_east)',xx_east_2km,zz_east_2km);
    outputS_east(:,:,n)=griddata(xx_east_8km,zz_east_8km,double(obcsS_east)',xx_east_2km,zz_east_2km);
    outputU_east(:,:,n)=griddata(xx_east_8km,zz_east_8km,double(obcsU_east)',xx_east_2km,zz_east_2km);
    outputV_east(:,:,n)=griddata(xx_east_8km,zz_east_8km,double(obcsV_east)',xx_east_2km,zz_east_2km);

end

outputT_south=permute(outputT_south,[2 1 3]);
outputS_south=permute(outputS_south,[2 1 3]);
outputU_south=permute(outputU_south,[2 1 3]);
outputV_south=permute(outputV_south,[2 1 3]);

wrslice('OBCS_T_south_AS2km_19930101_20191231.bin',outputT_south,1,'real*4')
wrslice('OBCS_S_south_AS2km_19930101_20191231.bin',outputS_south,1,'real*4')
wrslice('OBCS_U_south_AS2km_19930101_20191231.bin',outputU_south,1,'real*4')
wrslice('OBCS_V_south_AS2km_19930101_20191231.bin',outputV_south,1,'real*4')

outputT_east=permute(outputT_east,[2 1 3]);
outputS_east=permute(outputS_east,[2 1 3]);
outputU_east=permute(outputU_east,[2 1 3]);
outputV_east=permute(outputV_east,[2 1 3]);

wrslice('OBCS_T_east_AS2km_19930101_20191231.bin',outputT_east,1,'real*4')
wrslice('OBCS_S_east_AS2km_19930101_20191231.bin',outputS_east,1,'real*4')
wrslice('OBCS_U_east_AS2km_19930101_20191231.bin',outputU_east,1,'real*4')
wrslice('OBCS_V_east_AS2km_19930101_20191231.bin',outputV_east,1,'real*4')





%kan=rdslice('OBCS_S_south_AS2km_19930101_20190101.bin',[2250,64,360],1,'real*4');
%%
% sea level change to volume in the boundary 

for i=1:length(daten(:,1))
    i

    testE=squeeze(ncread([out_path,'ArabianSea_daily_',num2str(daten(i,1)),num2str(daten(i,2),'%2.2d'),num2str(daten(i,3),'%2.2d'),'.nc'],'zos'));
    temp=testE(1:541,32:end);
    eta_mean(i)=nanmean(temp(:));
end

wrslice('eta_east.bin',kron(ones(1640,1),eta_mean-mean(eta_mean)),1,'real*4');
wrslice('eta_south.bin',kron(ones(2250,1),eta_mean-mean(eta_mean)),1,'real*4');

%%
for i=1:50
    i

    test=squeeze(ncread([out_path,'ArabianSea_daily_',num2str(daten(i,1)),num2str(daten(i,2),'%2.2d'),num2str(daten(i,3),'%2.2d'),'.nc'],'thetao',[1 1 1 1],[Inf Inf 1 1]));
    temp=test(1:541,42:end);
    test_mean(i)=nanmean(temp(:));
end



