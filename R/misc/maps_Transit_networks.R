


fig_all <-  ggplot() + 

			# hex
            geom_sf(data=baseareas2, aes(fill=pop_densitykm2_jenks), color=alpha("gray80", 0.5)) +
            scale_fill_brewer(palette = 'Purples', direction = 1) +
				
			# rail
			geom_sf(data=rail, aes(color=type), alpha=.5, show.legend = "line") +
            scale_color_manual(values=c("#ef3b2c", "#006d2c", "#08519c", 'red'), 
			                   labels=c("Commuter train", "Light Railway", "Metro", "National rail")) +
            labs(color='') +
            ggsn::scalebar(data=baseareas2,
                           dist = 10, dist_unit = "km", st.size=3, height=0.01,
                           transform = TRUE, location='bottomleft' ) +
            labs(fill= bquote('Density of\nResidents per'~ Km^2)) +
            theme_void() +
            theme(legend.position = c(.1, .74)) +
            guides(fill = guide_legend(order = 1), 
                   color = guide_legend(order = 2))


