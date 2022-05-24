## compare resource
## May 24, 2022


# paths -----
outputs_path      = '/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/calepa-cn/outputs'

## 
curr_resource_file     = 'field_resource_revised.csv'
prev_resource_file     = 'field_resource.csv'

## curr
curr_resource_data = fread(file.path(outputs_path, 'entry-model-results', curr_resource_file), header = T)
curr_resource_data = curr_resource_data[, c('doc_field_code', 'resource')]
setnames(curr_resource_data, "resource", "curr_resource")

## prev
prev_resource_data = fread(file.path(outputs_path, 'entry-model-results', prev_resource_file), header = T)
prev_resource_data = prev_resource_data[, c('doc_field_code', 'resource')]
setnames(prev_resource_data, "resource", "prev_resource")


## combine
comp_resources <- merge(curr_resource_data, prev_resource_data,
                        by = c('doc_field_code'),
                        all.x = T)

comp_resources[, diff := curr_resource - prev_resource]

ggplot(comp_resources, aes(x = prev_resource/1e6, y = curr_resource/1e6, group = doc_field_code)) +
  geom_point()

