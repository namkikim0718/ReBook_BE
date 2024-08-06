package com.be.rebook.members;

import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.data.jpa.repository.config.EnableJpaAuditing;

@EnableJpaAuditing
@SpringBootApplication
public class MembersApplication {
    public static void main(String[] args){
        SpringApplication.run(MembersApplication.class, args);
    }
}
