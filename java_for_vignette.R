Qualtrics.SurveyEngine.addOnload ( function () {
  // Set number of choices ;
  var numChoice = 5;
  
  // Vectors containing all attribute levels :
    var prob1Array = ["increase in shootings", "infrequent and unreliable water service"];
    var neighborsArray = ["started a WhatsApp group to share information", "solved privately", "contacted government", "did not organize"];
    var resp1Array = ["got to know neighbors better", "created a formal group"];
    var resp2Array = ["success", "failure"];
    var prob2Array = ["increase in shootings", "infrequent and unreliable water service"];
    // Fisher - Yates shuffle :
      function shuffle( array ){
        for ( var i = array.length - 1; i > 0; i--) {
          var j = Math.floor ( Math.random() * (i + 1) );
          var temp = array [i ];
          array [i] = array [j ];
          array [j] = temp ;
        }
        return array ;
      }
    
    // Shuffle a vector , choose the first entry :
      function shuffle_one ( theArray ){
        var out = shuffle ( theArray );
        var out = out [0];
        return ( out )
      };
    
    // Perform the randomization and save the result :
      for (i = 1; i <=numChoice; i++) {
        Qualtrics.SurveyEngine.setEmbeddedData("choice" +i+"_prob1_1" , shuffle_one(prob1Array)) ;
        Qualtrics.SurveyEngine.setEmbeddedData("choice" +i+"_prob1_2" , shuffle_one(prob1Array)) ;
        Qualtrics.SurveyEngine.setEmbeddedData("choice" +i+"_neighbors_1" , shuffle_one(neighborsArray)) ;
        Qualtrics.SurveyEngine.setEmbeddedData("choice" +i+"_neighbors_2" , shuffle_one(neighborsArray)) ;
        Qualtrics.SurveyEngine.setEmbeddedData("choice" +i+"_resp1_1" , shuffle_one(resp1Array)) ;
        Qualtrics.SurveyEngine.setEmbeddedData("choice" +i+"_resp1_2" , shuffle_one(resp1Array)) ;
        Qualtrics.SurveyEngine.setEmbeddedData("choice" +i+"_resp2_1" , shuffle_one(resp2Array)) ;
        Qualtrics.SurveyEngine.setEmbeddedData("choice" +i+"_resp2_2" , shuffle_one(resp2Array)) ;
        Qualtrics.SurveyEngine.setEmbeddedData("choice" +i+"_prob2_1" , shuffle_one(prob2Array)) ;
        Qualtrics.SurveyEngine.setEmbeddedData("choice" +i+"_prob2_2" , shuffle_one(prob2Array)) ;
      }
}) ;

Qualtrics.SurveyEngine.addOnReady(function()
{
  /*Place your JavaScript here to run when the page is fully displayed*/
    
});

Qualtrics.SurveyEngine.addOnUnload(function()
{
  /*Place your JavaScript here to run when the page is unloaded*/
    
});