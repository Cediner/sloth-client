java -Xmx1024m -Dhttp.agent="curl/7.51.0" -jar hafen-updater.jar update https://raw.githubusercontent.com/Cediner/sloth-client/sloth/update/ -Djava.util.logging.config.file=logging.properties -Xmx1024M -jar hafen.jar