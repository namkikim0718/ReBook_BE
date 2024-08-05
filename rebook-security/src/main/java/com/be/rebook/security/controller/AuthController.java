package com.be.rebook.security.controller;

import com.be.rebook.domain.members.entity.Members;
import com.be.rebook.security.service.JoinService;
import com.be.rebook.security.dto.JoinDTO;
import com.be.rebook.global.config.BaseResponse;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.slf4j.LoggerFactory;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

import org.slf4j.Logger;

@RestController
public class AuthController {

    private static final Logger joinLogger = LoggerFactory.getLogger(AuthController.class);
    private final JoinService joinService;

    public AuthController(JoinService joinService){
        this.joinService = joinService;
    }

    @PostMapping("/join")
    public ResponseEntity<BaseResponse<Members>> joinProcess(@RequestBody JoinDTO joinDTO) {
        joinLogger.info(joinDTO.getUsername());
        return ResponseEntity.ok().body(new BaseResponse<>(joinService.joinProcess(joinDTO)));
    }

    @PostMapping("members/refreshtoken")
    public ResponseEntity<BaseResponse<RefreshTokens>> reissue(HttpServletRequest request, HttpServletResponse response) {
        return ResponseEntity.ok().body(new BaseResponse<>(reissueService.reissueToken(request, response)));
    }
}