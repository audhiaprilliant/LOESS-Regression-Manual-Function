split.data.uniquecode = function(location,sep,split.number,name.file) {
  # Location
  cname = file.path(location)
  location.cname = data.frame(data.name = dir(cname))
  data.survey = read.csv(file = paste(cname,
                                      '/',
                                      location.cname[1,],
                                      sep = ''),
                         header = FALSE,
                         sep = sep)
  # Make Index
  if (dim(data.survey)[1] > split.number) {
    bottom.trace = seq(from = 1,
                       to = floor(dim(data.survey)[1]/split.number)*split.number,
                       by = split.number)
    up.trace = seq(from = split.number,
                   to = floor(dim(data.survey)[1]/split.number)*split.number,
                   by = split.number)
    # Function for convert the data
    for(i in 1:length(up.trace)) {
      data.split = data.survey[bottom.trace[i]:up.trace[i],]
      write.table(x = data.split,
                file = paste(location,
                             '/',
                             name.file,
                             '_',
                             i,
                             '.csv',
                             sep = ''),
                row.names = FALSE,
                col.names = FALSE)
    }
    index.start = floor(dim(data.survey)[1]/split.number)*split.number + 1
    data.remain = data.survey[index.start:dim(data.survey)[1],]
    write.table(x = data.remain,
              file = paste(location,
                           '/',
                           name.file,
                           '_Remaining.csv',
                           sep = ''),
              row.names = FALSE,
              col.names = FALSE)
  }
  else {
    write.table(x = data.survey,
              file = paste(location,
                           '/',
                           name.file,
                           '_',
                           'little.csv',
                           sep = ''),
              row.names = FALSE,
              col.names = FALSE)
  }
}
# Check Function
split.data.uniquecode(location = 'to/your/dir/',     # Path to data directory
                      sep = ';',                     # Separator
                      split.number = 20000,          # Number of split rows
                      name.file = 'Custom Filename'  # Custom name for final data
                      )
