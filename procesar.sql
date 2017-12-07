PROCEDURE procesar IS

  vEmpresa              empresas.nombre%TYPE;
  vSuc                  sucursales.succonnum%TYPE;
  vDiferencia           NUMBER;
  vSumCorteCaja			NUMBER := 0;
  vAlgo 				NUMBER;
	
  
			vHelp					VARCHAR2(10);
	
	vImpTotal	 						NUMBER DEFAULT 0;
	vBdCBanMovExito 			NUMBER DEFAULT 0;
	vClaveMovNum          NUMBER;
	
	vNumPoliza 						VARCHAR2(200);
	vTarjeta   						VARCHAR2(200);
	vPreferencias         VARCHAR2(150);
	vObservaciones        VARCHAR2(1000);
	
	vFCC                  VARCHAR2(10);
	vEjercicioFCC         NUMBER(4);
  vPeriodoFCC           NUMBER(2);
	vConsecutivo					banmovimientos.consecutivo%TYPE;
	vConceptoCargo        detCjTiposPago.consobretasatj%TYPE;
	vMontoComiSobretasa		NUMBER DEFAULT 0;
	
	vIvaComision          NUMBER(1) DEFAULT 1;--INDICADOR PARA GENARAR IVA A COMISION BANCARIA
	
	--	
	-- PARA MANEJO DE ARCHIVOS
	--	
  vArchivo   						Text_io.file_type;
	
	CURSOR curEmpresa IS
	  SELECT nombre
	  FROM   empresas
	  WHERE  empnum = :parameter.empresa;
	
	CURSOR curInfoPoliza  (pcEjercicio   IN    Number
	                      ,pcPeriodo     IN    Number
	                      ,pcTipoPol     IN    Varchar2 )IS
		SELECT tipopolclave || '-' || diarionum || ' de ' || min(polnum) || '-' || max(polNum) poliza
		FROM   polizas
		WHERE  empnum          = :parameter.empresa
		AND    Ejercicio       = pcEjercicio
		AND    Periodo         = pcPeriodo
		AND    tipopolclave    = pctipopol
--		AND    diarionum       = pcDiario
		AND    referencia   LIKE :dummy.sucursal||:dummy.cortecaja
		GROUP BY tipopolclave, diarionum;
		
	--	
	-- PROCESO QUE SE AGREGO PARA EL CASO AMEX
	--
	
	cMontoCuadre         CONSTANT DetPolizas.Monto%TYPE := 0.05;
	
	vEjercicio   	       detpolizas.EJERCICIO%TYPE;
	vPeriodo     	       detpolizas.PERIODO%TYPE;	
	vDiarioNum   	       detpolizas.DIARIONUM%TYPE;
	vTipoPolClave	       detpolizas.TIPOPOLCLAVE%TYPE;
	vPolnum      	       detpolizas.POLNUM%TYPE;
	vPrePolnum  	       detpolizas.POLNUM%TYPE;
							         
	vSigDetPolNum	       DetPolizas.DetPolNum%type;
	vReferencia          detprepolizas.referencia%type;
	vDescripcion         detprepolizas.descripcion%type;
		  
	vTipomovimientoCom   detconcepabono.tipomovimiento%type;
  vCtaConClaveCOM      ctasbancarias.ctaconclave%type;
  vcencosCOM			     ctasbancarias.cencosnum%type;
  vSucconnumCom        ctasbancarias.succonnum%type;
  vsucCOM				       ctasbancarias.succonnum%type;
  vproyCOM             ctasbancarias.proynum%type;
  vinterCOM            ctasbancarias.intercia%type;
  vprodCOM             ctasbancarias.prodnum%type;
  vfutCOM              ctasbancarias.futuronum%type;
                                                    
	vTipomovimientoComMSI   detconcepabono.tipomovimiento%type;
  vCtaConClaveCOMMSI      ctasbancarias.ctaconclave%type;
  vcencosCOMMSI			      ctasbancarias.cencosnum%type;
  vSucconnumComMSI        ctasbancarias.succonnum%type;
  vsucCOMMSI				      ctasbancarias.succonnum%type;
  vproyCOMMSI             ctasbancarias.proynum%type;
  vinterCOMMSI            ctasbancarias.intercia%type;
  vprodCOMMSI             ctasbancarias.prodnum%type;
  vfutCOMMSI              ctasbancarias.futuronum%type;
                                                    
	vTipomovimientoIVA   detconcepabono.tipomovimiento%type;
	vCtaConClaveIVA      ctasbancarias.ctaconclave%type;
	vcencosIVA			     ctasbancarias.cencosnum%type;
	vSucconnumIVA        ctasbancarias.succonnum%type;
	vsucIVA				       ctasbancarias.succonnum%type;
	vproyIVA             ctasbancarias.proynum%type;
	vinterIVA            ctasbancarias.intercia%type;
	vprodIVA             ctasbancarias.prodnum%type;
	vfutIVA              ctasbancarias.futuronum%type;
	vTotalCargos         polizas.montocargos%type;
	vTotalAbonos	       polizas.montoabonos%type;
	vMontoIVA            detpolizas.monto%type;
	vAfiliacionSucursal  afiliacionesbanc.afiliacionbanc%type;
	vSrvFile             VARCHAR2(200);
 	vDirectorioTmp       VARCHAR2(1000);
 	vAmexMSI						 NUMBER;	
									
	CURSOR curNumPoliza(pCtaBanClave BANMOVIMIENTOS.ctabanclave%TYPE,
	                    pConsecutivo BANMOVIMIENTOS.consecutivo%TYPE,
	                    pClavemovnum BANMOVIMIENTOS.clavemovnum%TYPE) IS			
	  SELECT POLNUM,DIARIONUM,EJERCICIO, PERIODO, TIPOPOLCLAVE
		FROM   BANMOVIMIENTOS BM
		WHERE  bm.ctabanclave = pCtaBanClave
		AND    bm.empnum      = :parameter.empresa
  	AND    bm.clavemovnum = pClavemovnum   
		AND    bm.consecutivo = pConsecutivo;
																			   																		
	CURSOR curCtasBancarias(pPorcentaje NUMBER) IS							   									 
		SELECT tipomovimiento,ctaconclave,cencosnum,succonnum,proynum, intercia, prodnum, futuronum
		FROM   conceptosabono co, detconcepabono dco
		WHERE	 co.empnum          = :parameter.empresa
		AND		 co.empnum 	 	      = dco.empnum
		AND	   co.conabonum       = dco.conabonum
		AND    porcentaje         = pPorcentaje
		AND    sisnum             = 2
		AND    SUBSTR(nombre,1,4) = 'AMEX';

	CURSOR CurAmexMSI IS	
		Select  count(*)  Existe 
		from detcobros d , Cobros c
		Where  c.empnum   = d.empnum 
		and    c.succlave = d.succlave 
		and    c.cobfolio = d.cobfolio
		and    d.usacomisionesp = 1 
		and    d.msi      = 1
		and    c.empnum   = :parameter.empresa
		and    c.succlave = :dummy.sucursal
		AND    c.cortecaja= :dummy.cortecaja;
	
	CURSOR curSigDetPolNum (pEjercicio    detpolizas.ejercicio%TYPE,
	                        pPeriodo      detpolizas.periodo%TYPE,	                       
	                        pPolnum       detpolizas.polnum%TYPE,
	                        pDiarioNum    detpolizas.diarionum%TYPE,
	                        pTipoPolClave detpolizas.tipopolclave%TYPE) IS
    SELECT MAX(DETPOLNUM) + 1 SIGDETPOLNUM 
		FROM   DETPOLIZAS
		WHERE  empnum	      = :parameter.empresa
		AND    ejercicio 	  = pEjercicio
		AND    periodo 		  = pPeriodo
		AND    POLNUM			  = pPolnum
		AND    DIARIONUM		= pDiarioNum
		AND    TIPOPOLCLAVE = pTipoPolClave;
																
	CURSOR curSumDetPol   (pEjercicio    detpolizas.ejercicio%TYPE,
	                       pPeriodo      detpolizas.periodo%TYPE,	                       
	                       pPolnum       detpolizas.polnum%TYPE,
	                       pDiarioNum    detpolizas.diarionum%TYPE,
	                       pTipoPolClave detpolizas.tipopolclave%TYPE) IS
    SELECT SUM(DECODE(TIPOMOVIMIENTO,'C',MONTO,0)) MONTOCARGO,
		       SUM(DECODE(TIPOMOVIMIENTO,'A',MONTO,0)) MONTOABONO
    FROM   DETPOLIZAS
		WHERE  empnum		    = :parameter.empresa
		AND    ejercicio 	  = pEjercicio
		AND    periodo 		  = pPeriodo
		AND    POLNUM		    = pPolnum
		AND    DIARIONUM		= pDiarioNum
		AND    TIPOPOLCLAVE = pTipoPolClave;
									
	CURSOR CurDetPoliza   (pEjercicio    detpolizas.ejercicio%TYPE,
	                       pPeriodo      detpolizas.periodo%TYPE,	                       
	                       pPolnum       detpolizas.polnum%TYPE,
	                       pDiarioNum    detpolizas.diarionum%TYPE,
	                       pTipoPolClave detpolizas.tipopolclave%TYPE) IS
		SELECT *
		FROM   detpolizas 
		WHERE  empnum			    = :parameter.empresa
		AND    ejercicio 		  = pEjercicio
		AND    periodo 			  = pPeriodo
		AND    POLNUM				  = pPolnum
		AND    DIARIONUM			= pDiarioNum
		AND    TIPOPOLCLAVE	  = pTipoPolClave
		AND    TIPOMOVIMIENTO = 'A'
		AND    ROWNUM         = 1;
	
	CURSOR CurSigDetPrePolNum (pEjercicio    detprepolizas.ejercicio%TYPE,
	                           pPeriodo      detprepolizas.periodo%TYPE,	                       
	                           pPrePolnum    detprepolizas.polnum%TYPE,
	                           pDiarioNum    detprepolizas.diarionum%TYPE,
	                           pTipoPolClave detprepolizas.tipopolclave%TYPE) IS
		SELECT   MAX(DETPOLNUM)+1 SIGDETPOLNUM , referencia,descripcion
		FROM     DETPREPOLIZAS
		WHERE    empnum		    = :parameter.empresa
		AND      ejercicio 	  = pEjercicio
		AND      periodo 		  = pPeriodo
		AND      POLNUM			  = pPrePolNum
		AND      DIARIONUM		= pDiarioNum
		AND      TIPOPOLCLAVE = pTipoPolClave
		GROUP BY referencia, descripcion;
	
	CURSOR CurPrepolNum       (pEjercicio    detpolizas.ejercicio%TYPE,
	                           pPeriodo      detpolizas.periodo%TYPE,	                       
	                           pPolnum       detpolizas.polnum%TYPE,
	                           pDiarioNum    detpolizas.diarionum%TYPE,
	                           pTipoPolClave detpolizas.tipopolclave%TYPE) IS
	  SELECT prepolnum 
		FROM   prepolizas_polizas
		WHERE  empnum	      = :Parameter.empresa
		AND    ejercicio    =	pEjercicio   
		AND    periodo 	    = pPeriodo     
		AND    POLNUM		    =	pPolnum      
		AND    DIARIONUM	  =	pDiarioNum   
		AND    TIPOPOLCLAVE = pTipoPolClave;
		
	CURSOR curAfiliacionSucursal(pcEmpNum empresas.empnum%TYPE, pcSucClave sucursales.succlave%TYPE) IS
		SELECT afiliacionbanc
    FROM   afiliacionesbanc
    WHERE  empnum   = pcEmpNum
    AND    succlave = pcSucClave;
  
  CURSOR curComMesesPromoc(pEmpNum       empresas.empnum%TYPE,
                           pSucClave     sucursales.succlave%TYPE,
                           pCorteCaja    caja.cobros.cortecaja%TYPE) IS
  	SELECT 'COM' tipo,
  				 c.empnum,
  				 c.succlave,
  				 d.tipopago,
           0 tjcredito,
           ROUND(DECODE (d.MonedaExt, 1, D.MontoMonExt, d.monto) * (D.PorcentajeComision/100),2) ImporteTotal,
           d.monNum,
           m.nombre 		moneda,
           d.monedaext,
           d.tipocambio,
           DECODE (cjt.consolidacomision, 1, 99.99, d.PorcentajeComision) PorcentajeComision,
           dcjt.ctabanclave,
           NULL Banco,
           0 		GenPolSinComision,
           0 		ImpComision,
           0 		ImpIVA,
           0 		PorcentajeComisionR,
           d.nummesesprom 		NumMesesProm,
           d.porccomipromoc 	PorcComiPromoc,
           ROUND(DECODE (d.monedaext, 1, d.montomonext, d.monto)*(d.porccomipromoc/100),2) ImpComiPromoc,
           ROUND(DECODE (d.monedaext, 1, d.montomonext, d.monto)*(d.porccomipromoc/100)*(cjp.PorcComisionTj/100),2) ImpIvaPromoc,
           b.nombrecorto,
           d.msi,
           dcjt.consobretasatj
    FROM   detcobros 		d, 	cobros  c,  cjtipospago 	 cjt,  detCjTiposPago dcjt,
    			 masterbancos b, 	monedas m,  cajaparametros cjp
    WHERE  c.empnum            = pEmpNum
    AND    c.succlave          = pSucClave
    AND    c.cortecaja         = pCorteCaja
    AND    d.empnum            = c.empnum
    AND    d.succlave          = c.succlave
    AND    d.cobfolio          = c.cobfolio
    AND    d.porccomipromoc   != 0
    AND    d.PaisClave         = b.PaisClave(+)
    AND    d.bannum            = b.bannum(+)
    AND    d.monNum            = m.monNum
    AND    c.empnum            = cjp.empnum
    AND    cjt.empnum          = d.empnum
    AND    cjt.succlave        = d.succlave
    AND    cjt.TerminalNum     = d.TerminalNum
    AND    cjt.TPagoComision   = d.TPagoComision
    AND    dcjt.empnum         = d.empnum
    AND    dcjt.succlave       = d.succlave
    AND    dcjt.TerminalNum    = d.TerminalNum
    AND    dcjt.TPagoComision  = d.TPagoComision
    AND    dcjt.BanNum         = d.BanNum
    AND    dcjt.NumMesesProm   = d.NumMesesProm
    AND   ( ( b.genpolsincomision = 0 )
    					OR
    			  ( b.genpolsincomision = 1  AND  cjp.esquemapolcortecaja = 0 ) )
    AND    d.tipopago IN ('TJ','VM')
    AND   (
           ( c.status IN ('AU','US','UP') )
             OR
           ( c.status = 'CA'  AND  NVL(c.cortecaja,0) <> NVL(c.cortecajacanc,0) )
          );
    
	rgDetPolizas detpolizas%ROWTYPE;
	
BEGIN
	
	--
	-- NO SE PUDEN GENERAR LOS MOVIMIENTOS BANCARIOS CON MOVIMENTOS FUTUROS
	--
	
	IF (:parameter.BloquearMovimientosFuturos = 1) THEN
		LGNValid.execRollback;
		RAISE form_trigger_failure;
	END IF;
	
	IF get_block_property('POLIZA_LST',status) = lGNConst.cEdoNew THEN
		LGNValid.execRollback;
		RAISE form_trigger_failure;
	END IF;
	
	IF :dummy.usuclavealta IS NULL THEN
		LGNValid.execRollback;
		lgnerror.mensaje('Debe ingresar un cajero para procesar los movimientos');
		go_item('COBROS.USUCLAVEALTA');
		RAISE form_trigger_failure;
	END IF;
	
	IF :filtro.generaarchivo = 1 THEN
		
	  IF :filtro.ruta IS NULL THEN
	  	LGNValid.execRollback;
	  	LGNError.mensaje('El campo ruta es requerido');
	  	go_item('FILTRO.RUTA');
	  	RAISE form_trigger_failure;
	  END IF;
	  	
	  BEGIN
	    vDirectorioTmp := WebUtil_File_Transfer.Get_Work_Area();
	    vSrvFile       := vDirectorioTmp || '/' || 'TMPCJD25000' || TO_CHAR(SYSDATE, 'HH24MISSSSS') || '.txt';
	   	vArchivo       := Text_io.fopen(vSrvFile, 'W');
	   	
      Text_io.fclose(vArchivo);
		EXCEPTION
	  	WHEN OTHERS THEN
	   		LGNValid.execRollback;
	   		LGNerror.mensaje('Error al abrir archivo, la ruta es incorrecta');
	   		RAISE form_trigger_failure;
		END;
		
	END IF;
	
	IF NOT(LGNError.confirmacion('¿Está seguro de generar la póliza?')) THEN
		LGNValid.execRollback;
		RAISE form_trigger_failure;
	END IF;
	
	vEjercicioFCC := to_number(to_char(:parameter.fechacortecaja,'RRRR'));
  vPeriodoFCC   := to_number(to_char(:parameter.fechacortecaja,'MM'));
  vFCC          := to_char(:parameter.fechacortecaja, 'DD/MM/YYYY');
	
	--32739 los valores se toman tal cual sin hacer multiplicación por tipo de cambio
	/*go_block('CORTECAJA_LST');
	first_record;
	LOOP
		
		--vSumCorteCaja := vSumCorteCaja + (:cortecaja_lst.importetotal * :cortecaja_lst.tipocambio);
		vSumCorteCaja := vSumCorteCaja +  :cortecaja_lst.importetotal;
		EXIT WHEN :system.last_record = 'TRUE';
		next_record;
	END LOOP	
	first_record;
	
	--vDiferencia := vSumCorteCaja - :poliza_lst.total;
	*/
	--32739
	vDiferencia := :cortecaja_lst.total - :poliza_lst.total;
	
	
	IF nvl(vDiferencia, 0) = 0 THEN	
	  
	  IF (bdValidaCierreConta(:parameter.empresa
                           ,vEjercicioFCC
                           ,vPeriodoFCC) = 1) THEN
      LGNValid.execRollback;
      LGNError.Mensaje('El mes utilizado ya está cerrado en contabilidad');
      RAISE form_trigger_failure;
	  END IF;
	  
	  IF (:poliza_lst.tipopago IN ('TJ','VM','TJ-C','TJ-D','VM-C','VM-D')) AND nvl(:poliza_lst.PorcentajeComision,0) <> 0 THEN
   		
   		IF :parameter.PorcComisionTj <= 0 OR :parameter.PorcComisionTj IS NULL THEN	
     	  -- 
     	  -- BANDERA QUE INDICA QUE LA EMPRESA NO APLICA IVA SOBRE COMISIONES BANCARIAS (PANAMÁ, PETICIÓN DE GABRIEL ORTIZ)
     	  -- REQ. 4108
     	  -- 
     	  vIvaComision := 0 ;
      	
      	IF :parameter.esquemapolcortecaja = 1 THEN  				
   			  
   			  LGNValid.execRollback;
     	    LGNError.mensaje('Falta configurar el porcentaje de IVA para calcular la comisión de pagos con tarjeta de bancos con comisión especial');
     	    RAISE form_trigger_failure;
   			 
   			END IF;
   		
   		END IF;
   		
	  END IF;
	  
	END IF;
	
	
	
  FOR Emp	IN curEmpresa LOOP
    vEmpresa := Emp.nombre;
  END LOOP;
  
  -- 
  -- OBTENEMOS LA AFILIACIÓN BANCARIA DE LA SUCURSAL
  -- 
  OPEN  curAfiliacionSucursal(:parameter.empresa, :dummy.sucursal);
  FETCH curAfiliacionSucursal INTO vAfiliacionSucursal;
  CLOSE curAfiliacionSucursal;
  
	IF NVL(vDiferencia, 0) = 0 THEN
		
		go_block('POLIZA_LST');
		first_record;
		
		LOOP
   	  
   	  vPreferencias := :dummy.sucursal || :dummy.cortecaja;
   	  
   	  IF (:poliza_lst.observaciones IS NOT NULL) THEN
   		  :poliza_lst.observaciones := '-' || :poliza_lst.observaciones;
   	  END IF;
   	  
   	  IF (:poliza_lst.tipopago = bdCjConst.gcTpPg_Efectivo) THEN
   	  	
   		  IF (:poliza_lst.tipocambio = bdMonedaBaseEmp) THEN
   		 	  
   		 	  vObservaciones := vFCC  || '-' || 
   	                        'CC-' || :poliza_lst.tipopago || '-' || vPreferencias ||
   	                        :poliza_lst.observaciones;
   	    ELSE
   	      
   	      vObservaciones := vFCC  || '-' || 
   	                        'CC-' || :poliza_lst.tipopago || '-' || vPreferencias || '-' ||
   	                        to_char(:poliza_lst.importetotal) || '-' || nvl(:poliza_lst.tipocambio,bdMonedaBaseEmp) ||
   	                        :poliza_lst.observaciones;
   	    END IF;
   	    
   	  ELSIF :poliza_lst.tipopago IN ('TJ','VM') THEN
   	   
   	    vObservaciones := vFCC  || '-'|| 
   	                      'CC-' || :poliza_lst.tipopago || '-' || vPreferencias || '-' ||
   	                      nvl(:poliza_lst.nombrecorto, 'OTROS BANCOS') || '-' || 
   	                         vAfiliacionSucursal ||
   	                      :poliza_lst.observaciones;
   	                      
   	  ELSE
   	  	
   	    vObservaciones := vFCC  || '-'|| 
   	                      'CC-' || :poliza_lst.tipopago || '-' || vPreferencias || '-' ||
   	                      nvl(:poliza_lst.nombrecorto, 'OTROS BANCOS') ||
   	                      :poliza_lst.observaciones;
   	  END IF;
   	 
	    --
      -- SE VALIDAN EL EJERCICIO Y PERIODO A UTILIZAR, SE VALIDAN
      -- PARA REVISAR QUE LOS PERIODOS NO ESTEN CERRADOS
      --  

      IF (bdValidaCierreBancos(:parameter.empresa
                              ,vEjercicioFCC
                              ,vPeriodoFCC
                              ,:poliza_lst.ctabanclave) = 1) THEN
         
         LGNValid.execRollBack;
         LGNError.Mensaje('El mes a procesar ya esta cerrado en bancos');
         RAISE form_trigger_failure;
         
      END IF;

      BEGIN
      	
	    	IF (:poliza_lst.tipopago IN ('TJ','VM','TJ-C','TJ-D','VM-C','VM-D')) AND   	
   		 	   nvl(:poliza_lst.PorcentajeComision,0) <> 0                        THEN
   		 	   
   		 	   vTarjeta       := 'C'||:poliza_lst.tipopago ||'-'||:poliza_lst.PorcentajeComision;
   	       
   	       vObservaciones := vFCC || '-' || 
   	                         'CC-' || vTarjeta || '-' || vPreferencias || '-' ||
   	                         nvl(:poliza_lst.nombrecorto, 'OTROS BANCOS') || '-' || 
   	                         vAfiliacionSucursal ||
   	                         :poliza_lst.observaciones;
           
           -- 
     	     -- GENERACIÓN DEL MOVIMIENTO BANCARIO POR EL IMPORTE DE LA COMISIÓN
     	     -- 
     	     bdCreaBanMovs(:parameter.empresa                          -- EMPRESA
                        ,:poliza_lst.ctabanclave                     -- CUENTA BANCARIA
                        ,3                                           -- CLAVE DE MOVIMIENTO
                        ,vEjercicioFCC                               -- EJERCICIO
                        ,vPeriodoFCC                                 -- PERIODO
                        ,:parameter.Sistema                          -- SISTEMA
                        ,:parameter.fechacortecaja                   -- FECHA
                        ,:poliza_lst.importetotal                    -- IMPORTE
                        ,:poliza_lst.tipocambio                      -- TIPO DE CAMBIO
                        ,NULL                                        -- CLAVE DE BENEFICIARIO
                        ,vEmpresa                                    -- BENEFICIARIO
                        ,vPreferencias                               -- REFERENCIAS
                        ,vObservaciones                              -- OBSERVACIONES
                        ,NULL                                        -- CONCEPTO DE ABONO
                        ,:parameter.concarnumcom                     -- CONCEPTO DE CARGO
                        ,:dummy.sucursal                             -- SUCURSAL
                        ,0
                        ,vConsecutivo
                        ,null                                 );
           --
           -- GENERACIÓN DEL MOVIMIENTO BANCARIO POR EL IVA DE LA COMISIÓN
           --
           IF vIvaComision = 1 THEN
             
             --
             --SOLO SI ESTÁ CONFIGURADO EL % DE IVA SOBRE COMISIONES SE GENERAN LOS CARGOS BANCARIOS CORRESPONDIENTES
             --
            
             vTarjeta := 'I' || :poliza_lst.tipopago ||'-'|| :poliza_lst.porcentajecomision;
   	         
   	         vObservaciones := vFCC || '-' ||
   	                           'CC-'|| vTarjeta || '-' || vPreferencias || '-' ||
   	                           nvl(:poliza_lst.NombreCorto, 'OTROS BANCOS') || '-' ||
   	                           vAfiliacionSucursal ||
   	                           :poliza_lst.Observaciones;
             
		         bdCreaBanMovs(:parameter.Empresa
		                      ,:Poliza_LST.CtaBanclave
		                      ,3
		                      ,vEjercicioFCC
		                      ,vPeriodoFCC
		                      ,:parameter.Sistema
		                      ,:parameter.FechaCorteCaja
		                      ,:Poliza_LST.ImporteTotal *
		                       (:parameter.PorcComisionTj/100)
		                      ,:Poliza_LST.TipoCambio
		                      ,NULL
		                      ,vEmpresa
		                      ,vPreferencias
		                      ,vObservaciones
		                      ,NULL
		                      ,:parameter.ConCarIvaComTj
		                      ,:Dummy.Sucursal
		                      ,0
		                      ,vConsecutivo
                          ,null                                 );
           END IF;
					 	 
					 --
					 --COMISION DE MESES SIN INTERESES
					 --
					 
					   
	    	ELSE
					 
	    	   IF :poliza_lst.GenPolSinComision=1 AND :parameter.EsquemaPolCorteCaja=1 THEN
	    	   		IF nvl(:poliza_lst.PorcComiPromoc,0) != 0 THEN
	    	   			vMontoComiSobretasa := :poliza_lst.importetotal*(:poliza_lst.PorcComiPromoc/100);
	    	   		ELSE
	    	   			vMontoComiSobretasa := 0;	    	   		
	    	   		END IF;
	    	   		
	    	  		vImpTotal:=:poliza_lst.importetotal-round(((:poliza_lst.importetotal*(:poliza_lst.PorcentajeComisionR/100)) + vMontoComiSobretasa)*(1+(:parameter.PorcComisionTj/100)),2);
	    	   ELSE
	    	  		vImpTotal:=0;
	    	   END IF;
					
    	   	 FOR vContador IN 1..10000 LOOP     
							    
					   BEGIN    	   	            
    	   	  
    	   	     IF :poliza_lst.tipopago = 'TR' THEN
               	 vClaveMovNum := 8;
               ELSE
               	 vClaveMovNum := 2;
               END IF;
    	   	     
    	   	     vBdCBanMovExito := 0;
    	   	     
               bdCreaBanMovs(:parameter.empresa
							      	       ,:poliza_lst.ctabanclave
							      	       ,vClaveMovNum
						                 ,vEjercicioFCC
						                 ,vPeriodoFCC
							      	       ,:parameter.Sistema
							      	       ,:parameter.FechaCorteCaja
							      	       ,:poliza_lst.importetotal
							      	       ,nvl(:Poliza_LST.TipoCambio, 1)
							      	       ,NULL
							      	       ,vEmpresa
							      	       ,vPreferencias
							      	       ,vObservaciones
							      	       ,:parameter.pabono
							      	       ,NULL
							      	       ,:Dummy.Sucursal
							      	       ,vImpTotal
							      	       ,vConsecutivo
                             ,null                                 );
   	     
							 vBdCBanMovExito := 1;
							 
							 EXIT;
							    	          				    
						 EXCEPTION
						  
						   WHEN OTHERS THEN										
							  
							   IF SQLCODE = -00054 THEN										
								   LGNValid.execRollback;
							   ELSE
							   	 LGNValid.execRollback;
								   LGNError.mensaje('Error de bdCreaBanMovs = '||dbms_error_text||'|'||message_text||'|'||sqlerrm);
						       RAISE form_trigger_failure;
							   END IF;
							  
						 END;		
	    	
	    	   END LOOP;

	    	   IF vBdCBanMovExito = 0 THEN
	    			 
	    			 LGNValid.execRollback;
	    		   LGNError.Mensaje('Error de bdCreaBanMovs = Se encontraron Recursos Ocupados Reintente luego');		   
	    		   RAISE form_trigger_failure;
	    		
	    	   END IF;

           IF :poliza_lst.GenPolSinComision = 1 AND :parameter.EsquemaPolCorteCaja = 1 THEN
							 
		    	     FOR rPol IN curNumPoliza(:poliza_lst.ctabanclave,vConsecutivo,vClaveMovNum) LOOP
		    	       vPolnum       := rPol.polnum;
		    	       vDiarioNum    := rPol.diarionum;
		    	       vEjercicio    := rPol.ejercicio;
		    	       vPeriodo      := rPol.periodo;
		    	       vTipoPolClave := rPol.tipopolclave;	   		 
		    	     END LOOP;
		    	     
		    	     FOR DetPol IN CurSigDetPolNum(vEjercicio,
		    	   		       					             vPeriodo,		    	   		                      
		    	   		                				     vPolnum,
		    	   		                     				 vDiarioNum,		    	   		                      
		    	   		                     				 vTipoPolClave) LOOP
		    	       vSigDetPolNum := DetPol.sigdetpolnum;
		    	     END LOOP;
		    	   	 
		    	   	 vAmexMSI:= 0;
		    	     FOR Cta IN curCtasBancarias(3) LOOP
		    	     	    	   		
		    	       vTipoMovimientoComMSI := Cta.tipomovimiento;
		    	   		 vCtaConClaveComMSI := Cta.ctaconclave;
		    	   		 vcencosComMSI := Cta.cencosnum;	
		    	   		 vSucconnumComMSI := Cta.succonnum;
		    	   		 vproyComMSI := Cta.proynum;
		    	   		 vinterComMSI := Cta.intercia;
		    	   		 vprodComMSI := Cta.prodnum;
		    	   		 vfutComMSI := Cta.futuronum;
		    	   		 		    	   	  	   	 
		    	     END LOOP;
		    	   
		    	     IF vCtaConClaveComMSI IS NOT NULL AND vMontoComiSobretasa != 0  THEN   
		    	      
		    	        FOR regAmexmsi IN CurAmexMSI LOOP
		    	   	       vAmexMSI := regAmexmsi.Existe;
		    	   	    END LOOP;
		    	   	 
		    	   	 END IF;
		    	   	 		    	   	  	   	 		    	   		
		    	     FOR Cta IN curCtasBancarias(1) LOOP	    	   		
		    	       vTipoMovimientoCom := Cta.tipomovimiento;
		    	   		 vCtaConClaveCom := Cta.ctaconclave;
		    	   		 vcencosCom := Cta.cencosnum;	
		    	   		 vSucconnumCom := Cta.succonnum;
		    	   		 vproyCom := Cta.proynum;
		    	   		 vinterCom := Cta.intercia;
		    	   		 vprodCom := Cta.prodnum;
		    	   		 vfutCom := Cta.futuronum;
		    	     END LOOP;
																
		    	     FOR Cta IN CurCtasBancarias(2) LOOP	    	   		
		    	       vTipoMovimientoIVA := Cta.tipomovimiento;
		    	   		 vCtaConClaveIVA := Cta.ctaconclave;
		    	   		 vcencosIVA := Cta.cencosnum;	
		    	   		 vSucconnumIVA := Cta.succonnum;
		    	   		 vproyIVA := Cta.proynum;
		    	   		 vinterIVA := Cta.intercia;
		    	   		 vprodIVA := Cta.prodnum;
		    	   		 vfutIVA := Cta.futuronum;
		    	     END LOOP;
 								
 						   IF VSUCCONNUMCOM = '9999' then
		    	       vSuc   := bdObtenSucCon(:parameter.empresa, :dummy.Sucursal);
						   ELSE
 						     vSuc   := vsucconnumCOM;	   		
 						   END IF;
		    	   		 
	  	   		   FOR DetPol IN CurDetPoliza(vEjercicio,
		    	 		                  				  vPeriodo,		    	   		                      
		    	 		                  					vPolnum,
		    	 		                  					vDiarioNum,		    	   		                      
		    	 		                  					vTipoPolClave) LOOP
						     rgDetPolizas := DetPol;
						   END LOOP;
							
							IF vCtaConClaveComMSI IS NULL THEN  
								
								   INSERT INTO DETPOLIZAS( DETPOLNUM, EMPNUM, EJERCICIO
									                        , PERIODO,TIPOPOLCLAVE, DIARIONUM
									                        , POLNUM, FECHA, STATUS
									                        , TIPOMOVIMIENTO,MONTO
									                        , DESCRIPCION
									                        , REFERENCIA,COMPROBABLE,CTACONCLAVE
									                        , CENCOSNUM,SUCCONNUM,INTERCIA,
																					  FUTURONUM,PROYNUM,PRODNUM)
								   VALUES		           (vSigDetPolNum, :Parameter.empresa, vEjercicio
																          , vPeriodo,      vTipoPolClave, vDiarioNum
																          , vPolnum, rgDetPolizas.fecha, 'AB'
																          , vTipomovimientoCom,round((:poliza_lst.importetotal*(:poliza_lst.PorcentajeComisionR/100)) + vMontoComiSobretasa,2)
																          , rgDetPolizas.DESCRIPCION
																          ,	rgDetPolizas.REFERENCIA,rgDetPolizas.COMPROBABLE,vCtaConClaveCOM
																          , vcencosCOM    ,vsuc   ,vinterCOM
																          , vfutCOM       ,vproyCOM  ,vprodCOM);
						 	
						 	ELSE 
								
							   INSERT INTO DETPOLIZAS( DETPOLNUM, EMPNUM, EJERCICIO
									                        , PERIODO,TIPOPOLCLAVE, DIARIONUM
									                        , POLNUM, FECHA, STATUS
									                        , TIPOMOVIMIENTO,MONTO
									                        , DESCRIPCION
									                        , REFERENCIA,COMPROBABLE,CTACONCLAVE
									                        , CENCOSNUM,SUCCONNUM,INTERCIA,
																					  FUTURONUM,PROYNUM,PRODNUM)
								   VALUES		           (vSigDetPolNum, :Parameter.empresa, vEjercicio
																          , vPeriodo,      vTipoPolClave, vDiarioNum
																          , vPolnum, rgDetPolizas.fecha, 'AB'
																          , vTipomovimientoCom,round((:poliza_lst.importetotal*(:poliza_lst.PorcentajeComisionR/100)),2)
																          , rgDetPolizas.DESCRIPCION
																          ,	rgDetPolizas.REFERENCIA,rgDetPolizas.COMPROBABLE,vCtaConClaveCOM
																          , vcencosCOM    ,vsuc   ,vinterCOM
																          , vfutCOM       ,vproyCOM  ,vprodCOM);
																          
                                       
                   vSigDetPolNum:= vSigDetPolNum +1;

									   INSERT INTO DETPOLIZAS( DETPOLNUM, EMPNUM, EJERCICIO
										                        , PERIODO,TIPOPOLCLAVE, DIARIONUM
										                        , POLNUM, FECHA, STATUS
										                        , TIPOMOVIMIENTO,MONTO
										                        , DESCRIPCION
										                        , REFERENCIA,COMPROBABLE,CTACONCLAVE
										                        , CENCOSNUM,SUCCONNUM,INTERCIA,
																						  FUTURONUM,PROYNUM,PRODNUM)
									   VALUES		           (vSigDetPolNum, :Parameter.empresa, vEjercicio
																	          , vPeriodo,      vTipoPolClave, vDiarioNum
																	          , vPolnum, rgDetPolizas.fecha, 'AB'
																	          , vTipomovimientoCom,round(NVL(vMontoComiSobretasa,0),2)
																	          , rgDetPolizas.DESCRIPCION
																	          ,	rgDetPolizas.REFERENCIA,rgDetPolizas.COMPROBABLE,vCtaConClaveCOMMSI
																	          , vcencosCOMMSI    ,vsuc   ,vinterCOMMSI
																	          , vfutCOMMSI       ,vproyCOMMSI  ,vprodCOMMSI);
																
							END IF;						
 						
 						   IF VSUCCONNUMIVA = '9999' then
		    	       vSuc   := bdObtenSucCon(:parameter.empresa, :dummy.Sucursal);
 						   ELSE
 						     vSuc   := vsucconnumIVA;
		    	     END IF;
						
		    	     FOR SumDetPol IN CurSumDetPol(vEjercicio,
		    	   		                						 vPeriodo,		    	   		                      
		    	   		                						 vPolnum,
		    	   		                						 vDiarioNum,		    	   		                      
		    	   		                						 vTipoPolClave) LOOP
		    	       vTotalCargos := SumDetPol.montocargo;
		    	       vTotalAbonos := SumDetPol.montoabono;
		    	     END LOOP;
							 
													 
						   IF vTotalAbonos - 
						     (vTotalCargos + round( ((:poliza_lst.importetotal
									                        *(:poliza_lst.PorcentajeComisionR/100)) + vMontoComiSobretasa)
									                        *(:parameter.PorcComisionTj/100)
									                      ,2)) <= cMontoCuadre THEN
						
								 vMontoIVA :=  vtotalAbonos 
									                      -(vTotalCargos + round(((:poliza_lst.importetotal
									                      *(:poliza_lst.PorcentajeComisionR/100)) + vMontoComiSobretasa)
									                      *(:parameter.PorcComisionTj/100),2));
								 	                 
								 vMontoIVA := vMontoIVA + round(((:poliza_lst.importetotal
									                           *(:poliza_lst.PorcentajeComisionR/100)) + vMontoComiSobretasa)
									                           *(:parameter.PorcComisionTj/100),2);
								
						   ELSE
									 
                 vMontoIVA := round(((:poliza_lst.importetotal
									                   *(:poliza_lst.PorcentajeComisionR/100)) + vMontoComiSobretasa)
									                   *(:parameter.PorcComisionTj/100),2);
								
						  END IF;
																			
						  INSERT INTO DETPOLIZAS(DETPOLNUM, EMPNUM, EJERCICIO
								                      , PERIODO,TIPOPOLCLAVE,DIARIONUM
								                      , POLNUM, FECHA, STATUS
								                      , TIPOMOVIMIENTO,MONTO
								                      , DESCRIPCION
								                      , REFERENCIA,COMPROBABLE,CTACONCLAVE
								                      , CENCOSNUM,SUCCONNUM,INTERCIA
								                      , FUTURONUM,PROYNUM,PRODNUM)
						  VALUES		            (vSigDetPolNum+1, :Parameter.empresa, vEjercicio
														          , vPeriodo,       vTipoPolClave,     vDiarioNum
														          , vPolnum,        rgDetPolizas.fecha, 'AB'
														          , vTipomovimientoIVA, vMontoIVA
														          , rgDetPolizas.DESCRIPCION
														          , rgDetPolizas.REFERENCIA,rgDetPolizas.COMPROBABLE,vCtaConClaveIVA
														          , vcencosIVA     ,vsuc      ,vinterIVA
														          , vfutIVA        ,vproyIVA  ,vprodIVA);
																														
						  UPDATE POLIZAS
						  SET    montoabonos  = montocargos
						  WHERE  empnum	      = :parameter.empresa
						  AND    ejercicio    = vEjercicio
						  AND    periodo 	    = vPeriodo
						  AND    POLNUM		    = vPolnum
						  AND    DIARIONUM	  = vDiarioNum
						  AND    TIPOPOLCLAVE	= vTipoPolClave;
						      	   		
	  	   		  FOR DetPol IN CurDetPoliza(vEjercicio,
		    	     		       								 vPeriodo,		    	   		                      
		    	     		       								 vPolnum,
		    	     		       								 vDiarioNum,		    	   		                      
		    	     		       								 vTipoPolClave) LOOP
						    rgDetPolizas := DetPol;
						  END LOOP;
						  
						  FOR PrePol IN CurPrepolNum(vEjercicio,
		    	     		                			 vPeriodo,		    	   		                      
		    	     		                			 vPolnum,
		    	     		                			 vDiarioNum,		    	   		                      
		    	     		                			 vTipoPolClave) LOOP
						    vPrePolNum := prePol.prepolnum;
						  END LOOP;
						  
						  FOR DetPrePol IN CurSigDetPrePolNum(vEjercicio,
		    	     		                								vPeriodo,		    	   		                      
		    	     		                								vPrePolNum,
		    	     		                								vDiarioNum,		    	   		                      
		    	     		                								vTipoPolClave) LOOP
		    	      vSigDetPolNum := DetPrePol.sigdetpolnum;
		    	      vReferencia := DetPrePol.referencia;
		    	      vDescripcion := DetPrePol.descripcion;
		    	    END LOOP;
		    	     		
 						  IF VSUCCONNUMCOM = '9999' then
		    	      vSuc   := bdObtenSucCon(:parameter.empresa, :dummy.Sucursal);
						  ELSE
 						    vSuc   := vsucconnumCOM;	   		
						  END IF;
					
							IF vCtaConClaveComMSI IS NULL  THEN  
	  
				    	    INSERT INTO DETPREPOLIZAS(DETPOLNUM, EMPNUM, EJERCICIO
				    	     		                     ,PERIODO,TIPOPOLCLAVE,DIARIONUM
				    	     		                     , POLNUM, FECHA, STATUS
				    	     		                     , TIPOMOVIMIENTO,MONTO
				    	     		                     , DESCRIPCION
				    	     		                     , REFERENCIA,COMPROBABLE,CTACONCLAVE
				    	   		                       , CENCOSNUM,SUCCONNUM,INTERCIA
				    	   		                       , FUTURONUM,PROYNUM,PRODNUM, REFERENCIA2)
								  VALUES		              (vSigDetPolNum, :Parameter.empresa, vEjercicio
																          ,vPeriodo,vTipoPolClave,vDiarioNum
																          , vPrePolNum, rgDetPolizas.fecha, 'AP'
																          , vTipomovimientoCom, round((:poliza_lst.importetotal*(:poliza_lst.PorcentajeComisionR/100)) + vMontoComiSobretasa,2)
																          , vDESCRIPCION
																          ,	vREFERENCIA,rgDetPolizas.COMPROBABLE,vCtaConClaveCOM
																          , vcencosCOM     ,vsuc       ,vinterCOM
																          , vfutCOM        ,vproyCOM   ,vprodCOM,vREFERENCIA);
							ELSE 
			
				    	    INSERT INTO DETPREPOLIZAS(DETPOLNUM, EMPNUM, EJERCICIO
				    	     		                     ,PERIODO,TIPOPOLCLAVE,DIARIONUM
				    	     		                     , POLNUM, FECHA, STATUS
				    	     		                     , TIPOMOVIMIENTO,MONTO
				    	     		                     , DESCRIPCION
				    	     		                     , REFERENCIA,COMPROBABLE,CTACONCLAVE
				    	   		                       , CENCOSNUM,SUCCONNUM,INTERCIA
				    	   		                       , FUTURONUM,PROYNUM,PRODNUM, REFERENCIA2)
								  VALUES		              (vSigDetPolNum, :Parameter.empresa, vEjercicio
																          ,vPeriodo,vTipoPolClave,vDiarioNum
																          , vPrePolNum, rgDetPolizas.fecha, 'AP'
																          , vTipomovimientoCom, round((:poliza_lst.importetotal*(:poliza_lst.PorcentajeComisionR/100)),2)
																          , vDESCRIPCION
																          ,	vREFERENCIA,rgDetPolizas.COMPROBABLE,vCtaConClaveCOM
																          , vcencosCOM     ,vsuc       ,vinterCOM
																          , vfutCOM        ,vproyCOM   ,vprodCOM,vREFERENCIA);
				    	  
				    	    vSigDetPolNum := vSigDetPolNum +1 ;
				    	  
					    	    INSERT INTO DETPREPOLIZAS(DETPOLNUM, EMPNUM, EJERCICIO
					    	     		                     ,PERIODO,TIPOPOLCLAVE,DIARIONUM
					    	     		                     , POLNUM, FECHA, STATUS
					    	     		                     , TIPOMOVIMIENTO,MONTO
					    	     		                     , DESCRIPCION
					    	     		                     , REFERENCIA,COMPROBABLE,CTACONCLAVE
					    	   		                       , CENCOSNUM,SUCCONNUM,INTERCIA
					    	   		                       , FUTURONUM,PROYNUM,PRODNUM, REFERENCIA2)
									  VALUES		              (vSigDetPolNum, :Parameter.empresa, vEjercicio
																	          ,vPeriodo,vTipoPolClave,vDiarioNum
																	          , vPrePolNum, rgDetPolizas.fecha, 'AP'
																	          , vTipomovimientoCom, round(NVL(vMontoComiSobretasa,0),2)
																	          , vDESCRIPCION
																	          ,	vREFERENCIA,rgDetPolizas.COMPROBABLE,vCtaConClaveCOMMSI
																	          , vcencosCOMMSI     ,vsuc       ,vinterCOMMSI
																	          , vfutCOMMSI        ,vproyCOMMSI   ,vprodCOMMSI ,vREFERENCIA);
																	          
							END IF;
							
 					    IF VSUCCONNUMIVA = '9999' then
		    	      vSuc   := bdObtenSucCon(:parameter.empresa, :dummy.Sucursal);
 					    ELSE
 						    vSuc   := vsucconnumIVA;
 						  END IF;
                
						  INSERT INTO DETPREPOLIZAS(DETPOLNUM, EMPNUM, EJERCICIO
								                         ,PERIODO,TIPOPOLCLAVE, DIARIONUM
								                         , POLNUM, FECHA, STATUS
								                         , TIPOMOVIMIENTO,MONTO
								                         , DESCRIPCION
								                         , REFERENCIA,COMPROBABLE,CTACONCLAVE
								                         , CENCOSNUM,SUCCONNUM,INTERCIA
								                         , FUTURONUM,PROYNUM,PRODNUM,REFERENCIA2)
						  VALUES		 (vSigDetPolNum+1,  :Parameter.empresa   , vEjercicio
														          ,vPeriodo          ,vTipoPolClave       , vDiarioNum
														          , vPrePolNum       , rgDetPolizas.fecha , 'AP'
														          , vTipomovimientoIVA                    ,vMontoIVA
														          , vDESCRIPCION
														          , vREFERENCIA     ,rgDetPolizas.COMPROBABLE,vCtaConClaveIVA
														          , vcencosIVA      ,vsuc       ,vinterIVA
														          ,	vfutIVA         ,vproyIVA   ,vprodIVA,vREFERENCIA);
																																
						  UPDATE PREPOLIZAS
						  SET    montoabonos   = montocargos
						  WHERE  empnum	    = :parameter.empresa
						  AND    ejercicio     = vEjercicio
						  AND    periodo 	     = vPeriodo
						  AND    POLNUM	    	 =	vPrePolNum
						  AND    DIARIONUM     =	vDiarioNum
						  AND    TIPOPOLCLAVE	 =	vTipoPolClave;
	    	   		    	   		
	    	  END IF;
	    	        
	      END IF;
		
		  EXCEPTION
			
		  	WHEN OTHERS THEN
			    LGNValid.execRollback;
			    LGNError.Mensaje('Error de bdCreaBanMovs = '||dbms_error_text||'|'||message_text||'|'||sqlerrm);
			    RAISE form_trigger_failure;
	    END;
	  
	    EXIT WHEN :system.last_record  = 'TRUE';
	    next_record;
		  
	  END lOOP;
	  
	  
	  
	  
	  FOR rComisionesProm IN curComMesesPromoc(:parameter.empresa, :dummy.sucursal, :dummy.cortecaja)
    LOOP

			vTarjeta       := 'C'||rComisionesProm.tipopago ||'-'||rComisionesProm.PorcComiPromoc;
			
			vObservaciones := vFCC || '-' || 
			                 'CC-' || vTarjeta || '-' || vPreferencias || '-' ||
			                 nvl(rComisionesProm.nombrecorto, 'OTROS BANCOS') || '-' || 
			                 vAfiliacionSucursal ||
			                 :Filtro.observaciones;

	    vConceptoCargo := :parameter.concarnumcom;
	     
	    IF     NVL(rComisionesProm.MSI,0) = 1  and nvl(rComisionesProm.consobretasatj,0) != 0  THEN
	    	-- 
	    	--   Separar concepto de cargo para contabilizacion distinta de sobre tasa 
	    	--
	         vConceptoCargo:= rComisionesProm.consobretasatj;
	    END IF;
			
			--
			-- GENERACIÓN DEL MOVIMIENTO BANCARIO POR EL IMPORTE DE LA COMISIÓN
			--
			bdCreaBanMovs(:parameter.empresa                       -- EMPRESA
			          ,rComisionesProm.ctabanclave                 -- CUENTA BANCARIA
			          ,3                                           -- CLAVE DE MOVIMIENTO
			          ,vEjercicioFCC                               -- EJERCICIO
			          ,vPeriodoFCC                                 -- PERIODO
			          ,:parameter.Sistema                          -- SISTEMA
			          ,:parameter.fechacortecaja                   -- FECHA
			          ,rComisionesProm.impcomipromoc               -- IMPORTE SOBRECOMISION
			          ,rComisionesProm.tipocambio                  -- TIPO DE CAMBIO
			          ,NULL                                        -- CLAVE DE BENEFICIARIO
			          ,vEmpresa                                    -- BENEFICIARIO
			          ,vPreferencias                               -- REFERENCIAS
			          ,vObservaciones                              -- OBSERVACIONES
			          ,NULL                                        -- CONCEPTO DE ABONO
			          ,vConceptoCargo                              -- CONCEPTO DE CARGO
			          ,:dummy.sucursal                             -- SUCURSAL
			          ,0
			          ,vConsecutivo
			          ,null                                 );
			--
			-- GENERACIÓN DEL MOVIMIENTO BANCARIO POR EL IVA DE LA COMISIÓN
			--
			
			IF vIvaComision = 1 THEN 
			
			--
			--SOLO SI ESTÁ CONFIGURADO EL % DE IVA SOBRE COMISIONES SE GENERAN LOS CARGOS BANCARIOS CORRESPONDIENTES
			--
			              
				vTarjeta := 'I' || rComisionesProm.tipopago ||'-'|| rComisionesProm.PorcComiPromoc;
				 
				 vObservaciones := vFCC || '-' || 
				                   'CC-'|| vTarjeta || '-' || vPreferencias || '-' ||
				                   nvl(rComisionesProm.NombreCorto, 'OTROS BANCOS') || '-' || 
				                   vAfiliacionSucursal ||
				                   :Filtro.Observaciones;
				
				bdCreaBanMovs(:parameter.Empresa              
				            ,rComisionesProm.CtaBanclave         
				            ,3
				            ,vEjercicioFCC                   
				            ,vPeriodoFCC                     
				            ,:parameter.Sistema              
				            ,:parameter.FechaCorteCaja
				            ,rComisionesProm.impcomipromoc *       -- IMPORTE SOBRE COMISION
				             (:parameter.PorcComisionTj/100)
				            ,rComisionesProm.TipoCambio
				            ,NULL                            
				            ,vEmpresa           
				            ,vPreferencias                   
				            ,vObservaciones                  
				            ,NULL                            
				            ,:parameter.ConCarIvaComTj
				            ,:Dummy.Sucursal
				            ,0
				            ,vConsecutivo
				            ,null                                 );
				            
				END IF;
						 
			
    END LOOP;
    
    
    
    
    -- 
    -- SE REVISA QUE SEA UN ARCHIVO VÁLIDO
    -- 
		FOR InfoPol IN curInfoPoliza (NVL (vEjercicioFCC , vEjercicio )
		                             ,NVL (vPeriodoFCC   , vPeriodo   )
		                             ,'BN') LOOP
		  vNumPoliza := InfoPol.poliza;
		END LOOP;
    
    
    BEGIN
			
		  IF :filtro.generaarchivo = 1 THEN
				cargaArchivo(vNumPoliza);
		  END IF;
		  
	  EXCEPTION
			WHEN OTHERS THEN
				LGNValid.execRollback;
				LGNError.Mensaje('Error al grabar archivo = '||dbms_error_text||'|'||message_text||'|'||sqlerrm);
				RAISE form_trigger_failure;
	  END;
	  
	  
	  --post;
    BEGIN	
	  	UPDATE    Cobros
	  	SET       expcontcortecaja       = 1,
		  	     		fechaexpcontacortecaja = :parameter.fecha 
			WHERE     empnum   			         = :parameter.Empresa 
			AND       succlave   				     = :dummy.Sucursal 
			AND       (corteCaja  				   = :dummy.cortecaja 
			OR				 corteCajaCanc			   = :dummy.cortecaja) 
			AND       status                 IN ('AU','US','UP');

		  --RQ32739 deshacer
		  COMMIT;
		  --LGNValid.execRollback;
		
		EXCEPTION 
			WHEN OTHERS THEN
			  LGNValid.execRollback;
			  LGNError.Mensaje('Error al grabar datos = '||dbms_error_text||'|'||message_text||'|'||sqlerrm);
			  RAISE form_trigger_failure;
		END;
		
		
		:cobros.confirma := 0;
		:parameter.linea := 0;
		
		go_block('POLIZA_LST');
		clear_block(no_validate);
		
		go_block('CORTECAJA_LST');
		execute_query;
		
		activaItem('DUMMY.SUCURSAL', 		 				property_true,  property_false,  property_false);
    activaItem('DUMMY.USUCLAVEALTA', 				property_true,  property_false,  property_false);
    activaItem('DUMMY.CORTECAJA', 	 				property_true,  property_false,  property_false);
    activaItem('COBROS.ACEPTAR', 						property_true,  property_false,  property_false);
    activaItem('CORTECAJA_LST.CTABANCLAVE', property_true,  property_true,   property_false);
		
		go_block('FILTRO');
		clear_block(no_validate);
		
		go_block('CORTECAJA_LST');
		
		activaItem('FILTRO.LINEA', 					property_false,  property_false,  property_false);
		activaItem('FILTRO.BANCO', 					property_false,  property_false,  property_false);
		activaItem('FILTRO.IMPORTE', 				property_false,  property_false,  property_false);
		activaItem('FILTRO.CTABANCARIA', 		property_false,  property_false,  property_false);
		activaItem('FILTRO.OBSERVACIONES',	property_false,  property_false,  property_false);
		activaItem('FILTRO.ACEPTAR', 				property_false,  property_false,  property_false);
		activaItem('FILTRO.DESGLOZAR', 			property_false,  property_false,  property_false);
		activaItem('FILTRO.RUTA', 					property_false,  property_false,  property_false);
		activaItem('FILTRO.ABRIR', 					property_false,  property_false,  property_false);
		activaItem('FILTRO.GENERAARCHIVO',	property_false,  property_false,  property_false);
		activaItem('FILTRO.LIMPIA', 				property_false,  property_false,  property_false);
		activaItem('FILTRO.PROCESAR', 			property_false,  property_false,  property_false);
		  	
		LGNError.mensaje('Este Corte de Caja generó la Póliza: ' || vNumPoliza, 'ALERT_CAUTION1');
		
		go_block('CORTECAJA_LST');
		clear_block(no_validate);
	
	  :cobros.montototalcambioefectivo := NULL;
	  :filtro.ruta                     := NULL;
		:dummy.cortecaja                 := NULL;
				
		go_item ('DUMMY.CORTECAJA');
		
	ELSE
		
		IF vDiferencia IS NULL THEN 
			LGNError.mensaje('Error: Alguno de los importes no tiene valor');
		ELSE
			LGNError.mensaje('Error: Existe una diferencia de ' || vDiferencia);
		END IF;
	
	END IF;

END;
