package com.be.rebook.domain.members.controller;

import com.be.rebook.domain.members.entity.Members;
import com.be.rebook.domain.members.service.JoinService;
import com.be.rebook.domain.members.dto.JoinDTO;
import org.slf4j.LoggerFactory;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RestController;

import org.slf4j.Logger;

@RestController
public class JoinController {

    private static final Logger joinLogger = LoggerFactory.getLogger(JoinController.class);
    private final JoinService joinService;

    public JoinController(JoinService joinService){
        this.joinService = joinService;
    }

    //todo : BaseResponse 추가하기
    @PostMapping("/join")
    public ResponseEntity<Members> joinProcess(JoinDTO joinDTO) {
        joinLogger.info(joinDTO.getUsername());
        return joinService.joinProcess(joinDTO);
    }
}