<?php
	//require_once("../../login/config.php");
//	if(!$user->is_logged_in()) trigger_error("User not logged in !",E_USER_ERROR); 
	$serverpath = "/home/meteo/html/couches/";

	// GET ALL MODELS IN THE MODELS FOLDER
	$listmodels = glob($serverpath . "*", GLOB_ONLYDIR );
	
	// PROCESS EACH MODELS IN THE MODEL FOLDER
	foreach($listmodels as $modelfolder)
	{
		if ($modelfolder == 'bassins' || $modelfolder == 'regionmrc'){
		    continue;
		}
		
		// EACH DAYS OF THE MODEL	
		$listmodeldays = glob($modelfolder . "/*" ,GLOB_ONLYDIR);
		$listruns = array(); // CLEAR ARRAY FOR NEW VALUES
		foreach($listmodeldays as $days)
		{
			// EACH RUNS OF THE DAY
			$listdays = glob($days . "/*" , GLOB_ONLYDIR);
			$listhours = array(); // CLEAR ARRAY FOR NEW VALUES
			foreach($listdays as $hours)
			{

				$listhours[] = str_replace($days ."/", "", $hours);		
				
			}	

			$listruns[str_replace($modelfolder ."/","",$days)] = $listhours ;	
		}
		$list[str_replace($serverpath,"",$modelfolder)] = $listruns;	
	}

	echo json_encode($list);
?>
