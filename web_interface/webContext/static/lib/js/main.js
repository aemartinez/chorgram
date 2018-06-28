/**
 * Created by liu on 2017/4/3.
 */

/**
 * Set default props.
 */
      window.onload = function(){
                            $('#machines').hide();
                            $('#global').hide();
                            $('#ts').hide();
                            $('#textarea').val('');
                        }
/**
 * Define a file to save filename.
 * @type {String}
 */

                        var file=null;

/**
 * Define dots to save dot files.
 * @type {Array}
 */
                        var machinesDot=[];
                        var globalDot = [];
                        var tsDot = [];

/**
 * Init and reset function.
 */
                        function init()
                        {
                            $('#textarea').val('');
                            $('#result_global').attr("value",'');
                            $('#result_machines').attr("value",'');
                            $('#result_ts').attr("value",'');
                            $('#global').hide();
                            $('#machines').hide();
                            $('#ts').hide();
                            $('#selectfile').text('Default');
                            $('#error').hide();
                             $('#sum').prop("disabled",false);
                             $('#result_machines').children().remove();
                                if($('#result_ts').children().length>1){
                                $('#result_ts').children().remove();
                            }
                                $('#result_global').children().remove();
                            if(file !=null){
                                file=null;
                                 machinesDot=[];
                                 globalDot = [];
                                 tsDot = [];
                                //alert(file + "all clear");
                            }
                            if($('#upload')!=null)
                            {
                            $('#textarea_div').hide();
                            $('#upload').show();
                            $('#input-709').fileinput('reset');
                            $('#kv-success-1').html('<h4>Upload Status</h4><ul></ul>').hide();
                            }


                        }
   //Get the filename and read the content through ajax
						function showText(a)
						{
                             //init();//clear all
                                var filename=$(a).text();
                             $('#selectfile').text(filename);
                             file = filename;
                             $.ajax({
                                 url:"/showdata_ajax/",
                                 data:{'filename':filename},
                                 success: function (data) {

                                     //$("#textarea").text(data);
                                     $("#textarea").val(data);
                                 }
                             })

						}

/**
 * inputCommands function:
 * Get the text from textarea and send to view.py
 * If textarea is empty then show a alert.
 */
                        function inputCommands()
                        {

                            var textarea = $('#textarea').val();
                            if(textarea==null || textarea=="")
                            {
                                alert("Please input commands!");
                                return;
                            }
                            $.get("/textarea_ajax/", {'textarea':textarea}, function(ret){

                                        }).done(function(res) {
                                //Get file name from server.
                                //In input page, file name is default.
                                //In Example page, file name is the name of example which user clicked.
                                //In upload page, file name is the name of file which user uploaded.
                                file = res.toString();
                                if(file !=null){
                                     getGraph();
                                }

                            });
                        }
/**Generate graph function:
*1. According to file name could get a dot list after server executed shell commands.
*2. According the dot file name to get the content of this dot file.
*3. Using viz.js transform dot file to svg file.
*4. Using DOMParser() format a svg file to a DOM then, add the zoom function on DOM.
 * */

                          function getGraph()
                        {
                            //This alert only use in upload page.
                            if(file==null)
                            {
                                alert("Please choose a file!");
                                return ;
                            }
                            if($("#global").is(":visible"))
                            {
                                $('#global').hide();
                                $('#machines').hide();
                                $('#ts').hide();
                                $('#error').hide();
                                $('#result_machines').children().remove();

                                 if($('#result_ts').children().length>1){
                                $('#result_ts').children().remove();
                            }
                                $('#result_global').children().remove();
                            }
                            $.ajax({
                                url:"/get_file_return_dot/",
                                data:{'filename':file},
                                success: function (ret) {
                                     if(ret == 1)
                                        {
                                            $('#error').show();
                                        }
                                    var splitname=file.split('.');
                                    name = splitname[0];
                                    $.each(ret, function(index, item){
                                        //Testing read dot file
                                        //alert(item);
                                        if(item.toString().indexOf("dot")!=-1)
                                        {
                                            if(item.toString().indexOf("machines")!=-1)
                                            {
                                                var filename = item.toString();
                                                var dirName = name;
                                                 $.get("/get_dot/", {'filename':filename,'dirName':dirName}, function(ret){

                                                 }).done(function(data) {
                                                        //Push dot data for highlight
                                                        machinesDot.push(data);
                                                        //Display svg
                                                        var parser = new DOMParser();
                                                         var img = Viz(data,"svg");
                                                         var svg = parser.parseFromString(img,"image/svg+xml");
                                                         //Add zoom function on svg
                                                         addZoomforSvg(svg,"#result_machines");
                                                         $('#machines').show();
                                                         $('#sum').prop("disabled",ture);
                                                        });
                                            }


                                         if(item.toString().indexOf("ts")!=-1)
                                            {

                                                var filename = item.toString();
                                                var dirName = name;
                                                 $.get("/get_dot/", {'filename':filename,'dirName':dirName}, function(ret){

                                                 }).done(function(data) {
                                                     //push dot data for highlight
                                                        tsDot.push(data);
                                                        var parser = new DOMParser();
                                                         var img = Viz(data,"svg");
                                                         var svg = parser.parseFromString(img,"image/svg+xml")
                                                         addZoomforSvg(svg,"#result_ts");

                                                         $('#ts').show();
                                                        });
                                            }



                                            if(item.toString().indexOf("global")!=-1)
                                            {
                                                var filename = item.toString();
                                                var dirName = name;
                                                 $.get("/get_dot/", {'filename':filename,'dirName':dirName}, function(ret){

                                                 }).done(function(data) {
                                                     //push dot data for highlight
                                                        globalDot.push(data);
                                                        var parser = new DOMParser();
                                                         var img = Viz(data,"svg");
                                                         var svg = parser.parseFromString(img,"image/svg+xml")
                                                         addZoomforSvg(svg,"#result_global");

                                                         $('#global').show();
                                                        });
                                            }

                                        }

                                        });
                                }
                            });
                            if($('#upload')!=null)
                            {
                            showTextUpload();
                            $('#upload').hide();
                            }


                        }


/**
 * Add zoom function by addZoom()
 * @param svg: svg DOM data
 * @param container: DOM id
 */
                        function addZoomforSvg(svg,container)
                        {

                            svg.documentElement.setAttribute("onload","addZoom(this)");
                            $(container).append(svg.documentElement);

                        }

/**
 * Using svg-pan-zoom.js implement Zoom function on a svg.
 * @param com Dom object
 */
                     function addZoom(com)
                        {
                            window.panZoomTiger = svgPanZoom(com,{
                                zoomEnabled: true,
                              controlIconsEnabled: true,
                              fit: true,
                              center: true
                            });

                        }



/**
 * Get commands by line from text area.
 * Execution high light function.
 */
                        function getText()
                        {
                          var pos = getCursorPosition($('#textarea'));

                          var array = $('#textarea').val();
                          var col = 0;
                          for (var i=0; i<= pos;i++)
                          {
                            if(array[i] == '\n')
                            {
                              col ++;
                            }
                          }
                          //alert(col);
                          if(col !=0){
                            var codeArray = $('#textarea').val().split('\n');
                            var command = codeArray[col-1];
                            //alert (code);
                            //Invoke highlight function, value 1 is which line in text area,
                              //value 2 is the original dot file,
                              //value 3 is which graph.
                            highlightDot(command,machinesDot,"machines");
                            highlightDot(command,globalDot,"global");
                            highlightDot(command,tsDot,"ts");
                          }
                        }

/**
 * Get cursor position
 * @param ctrl: object
 * @returns {number}
 */
                        function getCursorPosition (ctrl) {
                            var CaretPos = 0;    // IE Support
                            if (document.selection) {
                            ctrl.focus ();
                                var Sel = document.selection.createRange ();
                                Sel.moveStart ('character', -ctrl.value.length);
                                CaretPos = Sel.text.length;
                            }
                            // Firefox support
                            else if (ctrl.prop("selectionStart") || ctrl.prop("selectionEnd") == '0')
                                CaretPos = ctrl.prop("selectionEnd");
                            return (CaretPos);
                        }

/**
 * According type to change dot file and display new a svg
 * In ts graph, it not only one dot file, so here using for loop.
 * @param command: String, which line
 * @param dotArray: Array, original dot file
 * @param type: String, which graph
 */
                        function highlightDot(command,dotArray,type)
                        {
                            var dot=null;
                            for(var i=0;i<dotArray.length;i++)
                            {
                                dot = dotArray[i];
                                if(type == "machines")
                                {
                                 changeMachine(dot,command);
                                }

                                if(type == "global")
                                {
                                 changeGlobal(dot,command);
                                }
                                if(type == "ts")
                                {
                                    changeTs(dot,command);
                                }
                            }

                        }

/**
 * Change machine dot file and display
 * find mapping line in dot file
 * the fsa file format should is determinate
 * .outputs A should get all nodes name include A
 * .marking q0 should get node q0
 * q0 1 ! hello q1 should get the path
 * Rule is:
 * commandArra[0] is first machine,
 * commandArra[2] is send or receive ; ! or ?,
 * commandarra[3] is message,
 * commandArra[4] is last machine.
 * @param dot: String, dot file
 * @param command: String, command
 * @returns {null}
 */
                        function changeMachine(dot,command)
                        {
                            var dotSliptArra = dot.split('\n');
                            var commandArra = command.split(" ");
                            var isOutputs = false;
                            var isMarking = false;
                            if(commandArra.length==2)
                            {
                                for(var i=0;i<commandArra.length;i++)
                                {
                                    if(commandArra[i].indexOf(".outputs")!=-1)
                                    {
                                        isOutputs= true;
                                    }
                                    if(commandArra[i].indexOf(".marking")!=-1)
                                    {
                                        isMarking = true;
                                    }
                                }
                            }

                            for (var i = 0; i< dotSliptArra.length;i++) {
                                var tempDot = dotSliptArra[i];
                                if(isOutputs==true)
                                {
                                    if(tempDot.indexOf(commandArra[1])!=-1 && tempDot.indexOf("[label")!=-1 )
                                    {
                                        tempDot = tempDot.substring(0,tempDot.length-2);
                                        dotSliptArra[i] = tempDot+',color=red];';

                                    }
                                }
                                if(isMarking==true)
                                {
                                    if(tempDot.indexOf(commandArra[1])!=-1 && tempDot.indexOf("style=filled")!=-1)
                                    {
                                        tempDot = tempDot.substring(0,tempDot.length-2);
                                        dotSliptArra[i] = tempDot+',color=red];';

                                    }
                                }
                                if(tempDot.indexOf(commandArra[0])!=-1 && tempDot.indexOf(commandArra[4])!=-1 && tempDot.indexOf(commandArra[2]) !=-1&&tempDot.indexOf(commandArra[3])!=-1){
                                    tempDot = tempDot.substring(0,tempDot.length-2);
                                    dotSliptArra[i] = tempDot+',color=red];';
                                }

                            }
                            var newdot = dotSliptArra.join('\n');
                            var parser = new DOMParser();
                            var img = Viz(newdot,"svg");
                            var svg = parser.parseFromString(img,"image/svg+xml")

                            $('#result_machines').children().remove();
                            addZoomforSvg(svg,"#result_machines");

                        }

/**
 * Change global dot file
 * for global: {q0 1 ! hello q1} and {q0 0 ? hello q1 }
 * both point to Tq0q0AM1hello [label="A &rarr; M1:hello", shape=box];
 * So, the common parts is including q0 hello and shape = box
 * @param dot
 * @param command
 * @returns {null}
 */
                        function changeGlobal(dot,command)
                        {
                            var dotSliptArra = dot.split('\n');
                            var commandArra = command.split(" ");
                             var isOutputs = false;

                             if(commandArra.length==2)
                            {
                                for(var i=0;i<commandArra.length;i++)
                                {
                                    if(commandArra[i].indexOf(".outputs")!=-1)
                                    {
                                        isOutputs= true;
                                    }

                                }
                            }
                             for (var i = 0; i< dotSliptArra.length;i++) {
                                var tempDot = dotSliptArra[i];
                                if(isOutputs== true)
                                {
                                    if(tempDot.indexOf(commandArra[1])!=-1 && tempDot.indexOf("shape=box")!=-1)
                                    {
                                        tempDot = tempDot.substring(0,tempDot.length-2);
                                        dotSliptArra[i] = tempDot+',color=red];';
                                    }
                                }
                                if(tempDot.indexOf(commandArra[0])!=-1 && tempDot.indexOf(commandArra[3])!=-1 && tempDot.indexOf("shape=box") !=-1){
                                    tempDot = tempDot.substring(0,tempDot.length-2);
                                    dotSliptArra[i] = tempDot+',color=red];';
                                }

                            }
                            var newdot = dotSliptArra.join('\n');
                            var parser = new DOMParser();
                            var img = Viz(newdot,"svg");
                            var svg = parser.parseFromString(img,"image/svg+xml")
                            $('#result_global').children().remove();
                            addZoomforSvg(svg,"#result_global");

                        }

/**
 * Change ts dot file
 * two kinds of graph ts0 ts
 * q0 1 ! hello q1 in ts0 is "q0_q0" -> "q1_q1"			[label="A&rarr;M1:hello"];
 * in ts is "q0_q0" -> "q1_q0____AM1hello"			[label="A&middot;M1 ! hello"];
 * In ts the message will display more than 1
 * @param dot
 * @param command
 * @returns {null}
 */
                        function changeTs(dot,command)
                        {
                            var dotSliptArra = dot.split('\n');
                            var commandArra = command.split(" ");
                            var isOutputs = false;
                            var isMarking = false;
                               if(commandArra.length==2)
                            {
                                for(var i=0;i<commandArra.length;i++)
                                {
                                    if(commandArra[i].indexOf(".outputs")!=-1)
                                    {
                                        isOutputs= true;
                                    }
                                     if(commandArra[i].indexOf(".marking")!=-1)
                                    {
                                        isMarking = true;
                                    }

                                }
                            }
                             for (var i = 0; i< dotSliptArra.length;i++) {
                                var tempDot = dotSliptArra[i];
                                if(isOutputs ==true){
                                if(tempDot.indexOf(commandArra[1])!=-1)
                                {
                                     tempDot = tempDot.substring(0,tempDot.length-2);
                                         dotSliptArra[i] = tempDot+',color=red];';
                                }
                                }
                                if(isMarking ==true)
                                {
                                    if(tempDot.indexOf(commandArra[1])!=-1 && tempDot.indexOf("arrowhead=dot")!=-1)
                                {
                                     tempDot = tempDot.substring(0,tempDot.length-2);
                                         dotSliptArra[i] = tempDot+',color=red];';
                                }
                                }
                                if(tempDot.indexOf(commandArra[0])!=-1 && tempDot.indexOf(commandArra[3])!=-1 && tempDot.indexOf(commandArra[4]) !=-1 && tempDot.indexOf("->") !=-1 )
                                {
                                    var times=compare(tempDot,commandArra[3]);
                                    if(times>2){


                                         if(tempDot.indexOf(commandArra[2])!=-1)
                                        {
                                         tempDot = tempDot.substring(0,tempDot.length-2);
                                         dotSliptArra[i] = tempDot+',color=red];';
                                        }
                                    }
                                    else
                                    {
                                         tempDot = tempDot.substring(0,tempDot.length-2);
                                         dotSliptArra[i] = tempDot+',color=red];';
                                    }
                                }

                            }
                            var newdot = dotSliptArra.join('\n');
                            var parser = new DOMParser();
                            var img = Viz(newdot,"svg");
                            var svg = parser.parseFromString(img,"image/svg+xml")
                            if($('#result_ts').children().length>1){
                                $('#result_ts').children().remove();
                            }
                            addZoomforSvg(svg,"#result_ts");

                        }

/**
 * Compare st1 between st2
 * Return is st2 times
 * @param str1
 * @param str2
 * @returns {number}
 */
                        function compare(str1,str2)
                        {
                            str1 = str1.replace(/\s/ig, "");
                            var str1array = str1.split("");
                            var str2array = str2.split("");
                            var times=0;
                            var count=0;
                            for(var i=0;i<str1array.length;i++)
                                for(var j=0;j<str2array.length;j++)
                                {
                                    if(str1array[i]==str2array[j])
                                    {
                                        count++;
                                        if(count == str2array.length)
                                        {
                                            times++;
                                            count=0;
                                        }
                                    }
                                }
                            return times;
                        }

/**
 *  upload page showTextUpload method
 *
 */
                function showTextUpload()
						{
                             //init();//clear all
                                var filename=file;

                             $.ajax({
                                 url:"/showdata_ajax/",
                                 data:{'filename':filename},
                                 success: function (data) {
                                    $("#textarea").val(data);
                                       $('#textarea_div').show();
                                 }
                             })

						}